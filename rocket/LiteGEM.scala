
import chisel3.util._
import chisel3._
import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.prci._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.system._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

import util._

case class LiteGEMParams(
  address: BigInt,
  size: Int = 0x1000)

case object PeripheryLiteGEMKey extends Field[Seq[LiteGEMParams]]

trait HasPeripheryLiteGEM { this: BaseSubsystem =>
  val eths = p(PeripheryLiteGEMKey) map { ps =>
    LiteGEMAttachParams(ps, PBUS,
      SynchronousCrossing(),
      SynchronousCrossing()).attachTo(this)
  }
  val ethNodes = eths.map(_.ioNode.makeSink())
}

trait HasPeripheryLiteGEMImp extends LazyModuleImp {
  val outer: HasPeripheryLiteGEM
  val eth = outer.ethNodes.zipWithIndex.map { case(n,i) => n.makeIO()(ValName(s"eth_$i")) }
}

////////////////////////////////////////////////////////////////////////////////
case class LiteGEMAttachParams(
  device: LiteGEMParams,
  controlWhere: TLBusWrapperLocation = PBUS,
  controlXType: ClockCrossingType = NoCrossing,
  intXType: ClockCrossingType = NoCrossing) {

  def attachTo(where: Attachable)(implicit p: Parameters): LiteGEM = where {
    val name = s"eth_${LiteGEMDevice.nextId()}"
    val tlbus = where.locateTLBusWrapper(controlWhere)
    val ethClockDomainWrapper = LazyModule(new ClockSinkDomain(take = None))
    val eth = ethClockDomainWrapper { LazyModule(new LiteGEM(tlbus.beatBytes, device)) }
    eth.suggestName(name)

    tlbus.coupleTo(s"device_named_${name}") { bus =>
      controlXType match {
        case _: SynchronousCrossing =>
          tlbus.dtsClk.map(_.bind(eth.device))
          ethClockDomainWrapper.clockNode := tlbus.fixedClockNode
        case _: RationalCrossing =>
          ethClockDomainWrapper.clockNode := tlbus.clockNode
        case _: AsynchronousCrossing =>
          val clockGroup = ClockGroup()
          clockGroup := where.asyncClockGroupsNode
          ethClockDomainWrapper.clockNode := clockGroup
        case _ => ()
      }

      (eth.controlXing(controlXType)
        := TLFragmenter(tlbus)
        := bus)
    }

    intXType match {
      case _: SynchronousCrossing  => where.ibus.fromSync     := eth.intXing(intXType)
      case _: RationalCrossing     => where.ibus.fromRational := eth.intXing(intXType)
      case _: AsynchronousCrossing => where.ibus.fromAsync    := eth.intXing(intXType)
      case _ => ()
    }

    val bus = where.locateTLBusWrapper(FBUS)
    eth.dmaClockNode := bus.clockNode
    bus.coupleFrom(s"port_named_$name") {
      _ := TLWidthWidget(tlbus.beatBytes) := eth.dmaNode
    }

    eth
  }
}

object LiteGEMDevice {
  val nextId = { var i = -1; () => { i += 1; i } }
  val width = 32.W
}

////////////////////////////////////////////////////////////////////////////////
class LiteGEMIn extends Bundle {
  val NCR    = UInt(32.W) // 0x000, Network conftrol
                          //        [ 2] RE      - Receive enable
                          //        [ 3] TE      - Transmit enable
                          //        [ 4] MPE     - Management port enable
                          //        [ 5] CLRSTAT - Clear status regs
                          //        [ 9] TSTART  - Start transmission
                          //        [10] THALT   - Transmission halt
  val NCFGR  = UInt(32.W) // 0x004, Network configuration
                          //        [18+:3] CLK - MDC clock division
                          //        [21+:2] DBW - Data bus width
  val USRIO  = UInt(32.W) // 0x00C, User IO
  val DMACFG = UInt(32.W) // 0x010, DMA configuration
                          //        [ 0] BFLDO      - Fixed burst length for DMA
                          //        [ 6] ENDIA_DESC - Endian swap mode for management descriptor access
                          //        [ 7] ENDIA_PKT  - Endian swap mode for packet access 
                          //        [10] TXPBMS     - Tx packet buffer memroy size select
                          //        [16] RXBS       - Receive buffer size
                          //        [30] ADDR64     - Address bus width 64b or 32b
  val RBQP   = UInt(32.W) // 0x018, RX Q Base Address
  val TBQP   = UInt(32.W) // 0x01C, TX Q Base Address
  val MAN    = UInt(32.W) // 0x034, PHY Maintenance
  val SA1B   = UInt(32.W) // 0x088, Specific address 1 bottom [31:0]
  val SA1T   = UInt(32.W) // 0x08C, Specific address 1 top [47:32]
  val SA2B   = UInt(32.W) // 0x090, Specific address 2 bottom [31:0]
  val SA2T   = UInt(32.W) // 0x094, Specific address 2 top [47:32]
  val SA3B   = UInt(32.W) // 0x098, Specific address 3 bottom [31:0]
  val SA3T   = UInt(32.W) // 0x09C, Specific address 3 top [47:32]
  val SA4B   = UInt(32.W) // 0x0A0, Specific address 3 bottom [31:0]
  val SA4T   = UInt(32.W) // 0x0A4, Specific address 3 top [47:32]
  val TBQPH  = UInt(32.W) // 0x4C8, TX Q Base Address
  val RBQPH  = UInt(32.W) // 0x4D4, RX Q Base Address
  val DCFG1  = UInt(32.W) // 0x280, Design configuration register 1
                          //        [25+:3] DBWDEF - Data bus width default
  val DCFG6  = UInt(32.W) // 0x294, Design configuration register 6
                          //        [23+:1] DAW64 - DMA Address Width 64
  
  val TBQP1  = UInt(32.W) // 0x440, TX Q Base Address - Queue 1
  val TBQP2  = UInt(32.W) // 0x444, TX Q Base Address - Queue 2
  val TBQP3  = UInt(32.W) // 0x448, TX Q Base Address - Queue 3
  val TBQP4  = UInt(32.W) // 0x44C, TX Q Base Address - Queue 4
  val TBQP5  = UInt(32.W) // 0x450, TX Q Base Address - Queue 5
  val TBQP6  = UInt(32.W) // 0x454, TX Q Base Address - Queue 6
  val TBQP7  = UInt(32.W) // 0x458, TX Q Base Address - Queue 7

  val RBQP1  = UInt(32.W) // 0x480, TX Q Base Address - Queue 1
  val RBQP2  = UInt(32.W) // 0x484, TX Q Base Address - Queue 2
  val RBQP3  = UInt(32.W) // 0x488, TX Q Base Address - Queue 3
  val RBQP4  = UInt(32.W) // 0x48C, TX Q Base Address - Queue 4
  val RBQP5  = UInt(32.W) // 0x490, TX Q Base Address - Queue 5
  val RBQP6  = UInt(32.W) // 0x494, TX Q Base Address - Queue 6
  val RBQP7  = UInt(32.W) // 0x498, TX Q Base Address - Queue 7
  
  val IER    = UInt(32.W) // 0x600, Interrupt Enable
  val IER1   = UInt(32.W) // 0x604, Interrupt Enable
  val IDR    = UInt(32.W) // 0x620, Interrupt Disable
  val IDR1   = UInt(32.W) // 0x624, Interrupt Disable
  val IMR    = UInt(32.W) // 0x640, Interrupt Mask
  val IMR1   = UInt(32.W) // 0x644, Interrupt Mask
}

class LiteGEMOut extends Bundle {
  val MID    = UInt(32.W) // 0x0FC, Module ID
                          //        [16+:12] ID NUM (GEM >= 0x2)
  val NSR    = UInt(32.W) // 0x008, Network status
                          //        [ 2] IDLE
  val TSR    = UInt(32.W) // 0x014, Transmit status
                          //        [ 3] TGO - Transmit go
}

class LiteGEMIO extends Bundle {
  val interrupts = Output(UInt(1.W))
  val gtx_clk    = Input (Clock())
  val tx_clk     = Output(Clock())
  val rx_clk     = Input (Clock())
  val rxd        = Input (UInt(8.W))
  val rx_dv      = Input (Bool())
  val rx_er      = Input (Bool())
  val txd        = Output(UInt(8.W))
  val tx_en      = Output(Bool())
  val tx_er      = Output(Bool())
  val status     = Input (Bool())
  val phy_ad     = Input (UInt(5.W))
  val phy_id     = Input (UInt(32.W))
}

class LiteGEM(busWidthBytes: Int, c: LiteGEMParams)(implicit p: Parameters)
  extends IORegisterRouter(
    RegisterRouterParams(
      name = "eth",
      compat = Seq("cdns,macb"),
      base = c.address,
      beatBytes = busWidthBytes),
    new LiteGEMIO)
  with HasInterruptSources
  with HasTLControlRegMap {

  override def extraResources(resources: ResourceBindings) = Map[String, Seq[ResourceValue]](
    "mac-address" -> Seq(0, 0, 0, 0, 0, 0).map(ResourceInt(_)),
    "phy-mode" -> Seq(ResourceString("gmii")))

  val dmaClockNode = ClockSinkNode(Seq(ClockSinkParameters()))
  val dmaNode = TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      name = "dma_port_tl", sourceId = IdRange(0, 4)) ))))

  def nInterrupts = 1
  lazy val module = new LazyModuleImp(this) {
    val dmaClock = dmaClockNode.in.head._1.clock
    val dmaReset = dmaClockNode.in.head._1.reset
    val dmaEdge = dmaNode.out.head._2
    val dma = dmaNode.out.head._1
    val ctl = controlNode.in.head._1

    val output = Reg(new LiteGEMOut)
    val inputs = RegInit({
      val init = 0.U.asTypeOf(new LiteGEMIn)
      init.DCFG6 := 0x800000.U // DAW64[Bit 23, Enable 64-bit DMA]
      init.TBQPH := 0x0.U
      init.RBQPH := 0x0.U
      init
    })

    val mapping = Seq(
      0x0FC -> Seq(RegField.r(32, output.MID   )), // 0x0FC, Module ID
      0x008 -> Seq(RegField.r(32, output.NSR   )), // 0x008, Network status
      0x014 -> Seq(RegField  (32, output.TSR   )), // 0x014, Transmit status

      0x000 -> Seq(RegField  (32, inputs.NCR   )), // 0x000, Network conftrol
      0x004 -> Seq(RegField  (32, inputs.NCFGR )), // 0x004, Network configuration
      0x00C -> Seq(RegField  (32, inputs.USRIO )), // 0x00C, User IO
      0x010 -> Seq(RegField  (32, inputs.DMACFG)), // 0x010, DMA configuration
      0x018 -> Seq(RegField  (32, inputs.RBQP  )), // 0x018, RX Q Base Address
      0x01C -> Seq(RegField  (32, inputs.TBQP  )), // 0x01C, TX Q Base Address
      0x088 -> Seq(RegField  (32, inputs.SA1B  )), // 0x088, Specific address 1 bottom [31:0]
      0x08C -> Seq(RegField  (32, inputs.SA1T  )), // 0x08C, Specific address 1 top [47:32]
      0x090 -> Seq(RegField  (32, inputs.SA2B  )), // 0x090, Specific address 2 bottom [31:0]
      0x094 -> Seq(RegField  (32, inputs.SA2T  )), // 0x094, Specific address 2 top [47:32]
      0x098 -> Seq(RegField  (32, inputs.SA3B  )), // 0x098, Specific address 3 bottom [31:0]
      0x09C -> Seq(RegField  (32, inputs.SA3T  )), // 0x09C, Specific address 3 top [47:32]
      0x0A0 -> Seq(RegField  (32, inputs.SA4B  )), // 0x0A0, Specific address 3 bottom [31:0]
      0x0A4 -> Seq(RegField  (32, inputs.SA4T  )), // 0x0A4, Specific address 3 top [47:32]
      0x034 -> Seq(RegField  (32, inputs.MAN   )), // 0x034, PHY Maintenance
      0x280 -> Seq(RegField  (32, inputs.DCFG1 )), // 0x280, Design configuration register 1
      0x294 -> Seq(RegField  (32, inputs.DCFG6 )), // 0x294, Design configuration register 6
      
      0x440 -> Seq(RegField  (32, inputs.TBQP1 )), // 0x440, TX Q Base Address - Queue 1
      0x444 -> Seq(RegField  (32, inputs.TBQP2 )), // 0x444, TX Q Base Address - Queue 2
      0x448 -> Seq(RegField  (32, inputs.TBQP3 )), // 0x448, TX Q Base Address - Queue 3
      0x44C -> Seq(RegField  (32, inputs.TBQP4 )), // 0x44C, TX Q Base Address - Queue 4
      0x450 -> Seq(RegField  (32, inputs.TBQP5 )), // 0x450, TX Q Base Address - Queue 5
      0x454 -> Seq(RegField  (32, inputs.TBQP6 )), // 0x454, TX Q Base Address - Queue 6
      0x458 -> Seq(RegField  (32, inputs.TBQP7 )), // 0x458, TX Q Base Address - Queue 7
      
      0x480 -> Seq(RegField  (32, inputs.RBQP1 )), // 0x480, RX Q Base Address - Queue 1
      0x484 -> Seq(RegField  (32, inputs.RBQP2 )), // 0x484, RX Q Base Address - Queue 2
      0x488 -> Seq(RegField  (32, inputs.RBQP3 )), // 0x488, RX Q Base Address - Queue 3
      0x48C -> Seq(RegField  (32, inputs.RBQP4 )), // 0x48C, RX Q Base Address - Queue 4
      0x490 -> Seq(RegField  (32, inputs.RBQP5 )), // 0x490, RX Q Base Address - Queue 5
      0x494 -> Seq(RegField  (32, inputs.RBQP6 )), // 0x494, RX Q Base Address - Queue 6
      0x498 -> Seq(RegField  (32, inputs.RBQP7 )), // 0x498, RX Q Base Address - Queue 7
      
      0x4C8 -> Seq(RegField  (32, inputs.TBQPH )), // 0x4C8, TX Q Base Address
      0x4D4 -> Seq(RegField  (32, inputs.RBQPH )), // 0x4D4, RX Q Base Address
      
      0x600 -> Seq(RegField  (32, inputs.IER   )), // 0x600, Interrupt Enable
      0x604 -> Seq(RegField  (32, inputs.IER1  )), // 0x604, Interrupt Enable
      0x620 -> Seq(RegField  (32, inputs.IDR   )), // 0x620, Interrupt Disable
      0x624 -> Seq(RegField  (32, inputs.IDR1  )), // 0x624, Interrupt Disable
      0x640 -> Seq(RegField  (32, inputs.IMR   )), // 0x640, Interrupt Mask
      0x644 -> Seq(RegField  (32, inputs.IMR1  ))  // 0x644, Interrupt Mask
    )
    regmap(mapping:_*)

    val eth = Module(new LiteGEMAdapter(beatBytes, dmaEdge))
    output := eth.io.ctl.out
    eth.io.ctl.in := inputs
    when (eth.io.man_dv) {
      inputs.MAN := eth.io.man_do
    }

    eth.io.port.phy_id  := Mux(port.phy_id.orR, port.phy_id, "h02000000".U)
    eth.io.port.phy_ad  := port.phy_ad
    eth.io.port.status  := port.status

    eth.io.port.gtx_clk := port.gtx_clk

    port.tx_clk         := eth.io.port.tx_clk
    port.tx_en          := eth.io.port.tx_en
    port.tx_er          := eth.io.port.tx_er
    port.txd            := eth.io.port.txd

    eth.io.port.rx_clk  := port.rx_clk
    eth.io.port.rx_er   := port.rx_er
    eth.io.port.rx_dv   := port.rx_dv
    eth.io.port.rxd     := port.rxd

    dma.a.valid         := eth.io.dma_a.valid
    dma.a.bits          := eth.io.dma_a.bits
    eth.io.dma_a.ready  := dma.a.ready

    dma.d.ready         := eth.io.dma_d.ready
    eth.io.dma_d.valid  := dma.d.valid
    eth.io.dma_d.bits   := dma.d.bits
  }
}

////////////////////////////////////////////////////////////////////////////////
class LiteGEMCtlIO extends Bundle {
  val out = Output(new LiteGEMOut)
  val in  = Input (new LiteGEMIn)
}

class LiteGEMAdapter(beatBytes: Int, dmaEdge: TLEdge) extends Module {
  val io = IO(new Bundle {
    val port = new LiteGEMIO
    val ctl = new LiteGEMCtlIO
    val man_dv = Output(Bool())
    val man_do = Output(UInt(32.W))
    val dma_a = Decoupled(new TLBundleA(dmaEdge.bundle))
    val dma_d = Flipped(Decoupled(new TLBundleD(dmaEdge.bundle)))
  })

  io.ctl.out         := 0.U.asTypeOf(io.ctl.out)
  io.ctl.out.MID     := 20000.U
  io.ctl.out.NSR     := io.port.status ## 0.U(2.W)
  io.port.interrupts := 0.U
 
  //----------------------------------------------------------------------------
  // Management Port (MDIO)
  //----------------------------------------------------------------------------
  val gem_man = Module(new LiteGEMMan)
  gem_man.io.phy_id  := io.port.phy_id
  gem_man.io.phy_ad  := io.port.phy_ad
  gem_man.io.status  := io.port.status
  gem_man.io.mpe     := io.ctl.in.NCR(4)
  gem_man.io.man_di  := io.ctl.in.MAN
  io.man_dv          := gem_man.io.man_dv
  io.man_do          := gem_man.io.man_do


  //----------------------------------------------------------------------------
  // TX Channel
  //----------------------------------------------------------------------------
  val gem_txd = Module(new LiteGEMTxD(beatBytes * 8))
  gem_txd.io.gtx_clk := io.port.gtx_clk
  io.port.tx_clk     := gem_txd.io.tx_clk
  io.port.tx_en      := gem_txd.io.tx_en
  io.port.tx_er      := gem_txd.io.tx_er
  io.port.txd        := gem_txd.io.txd

  gem_txd.io.tqa     := io.ctl.in.TBQPH ## io.ctl.in.TBQP
  gem_txd.io.tqe     := io.ctl.in.NCR(3) && io.ctl.in.NCR(9) // TE && TSTART

  //----------------------------------------------------------------------------
  // DMA Mux
  //----------------------------------------------------------------------------
  val gem_dma = Module(new LiteGEMDMA(dmaEdge.bundle, 1))
  gem_dma.io.dma_a.ready := io.dma_a.ready
  io.dma_a.valid := gem_dma.io.dma_a.valid
  io.dma_a.bits  := gem_dma.io.dma_a.bits

  io.dma_d.ready := gem_dma.io.dma_d.ready
  gem_dma.io.dma_d.valid := io.dma_d.valid
  gem_dma.io.dma_d.bits  := io.dma_d.bits

  gem_dma.io.dma_do(0) <> gem_txd.io.dma_di
  gem_dma.io.dma_di(0) <> gem_txd.io.dma_do
  gem_dma.io.dma_ad(0) := gem_txd.io.dma_ad
  gem_dma.io.dma_sz(0) := gem_txd.io.dma_sz
  gem_dma.io.dma_we(0) := gem_txd.io.dma_we
}

class LiteGEMMan extends Module {
  val io = IO(new Bundle {
    val phy_id = Input(UInt(32.W))
    val phy_ad = Input(UInt(5.W))
    val status = Input(Bool())
    val mpe = Input(Bool())
    val man_di = Input(UInt(32.W))
    val man_do = Output(UInt(32.W))
    val man_dv = Output(Bool())
  })
  val man_di = Mux(io.mpe, io.man_di(15, 0), 0.U(16.W))
  val man_do = RegNext(0.U(16.W))
  val man_dv = RegNext(0.B)

  val sof    = Mux(io.mpe, io.man_di(31,30), 0.U( 2.W))
  val rw     = Mux(io.mpe, io.man_di(29,28), 0.U( 2.W))
  val phya   = Mux(io.mpe, io.man_di(27,23), 0.U( 5.W))
  val rega   = Mux(io.mpe, io.man_di(22,18), 0.U( 5.W))

  val regs = Wire(Vec(16, UInt(16.W)))
  when (sof === 1.U && phya === io.phy_ad && !rega(4)) {
    when(rw === 2.U) { // read
      man_do := regs(rega)
      man_dv := 1.B
    } /*
    when (rw === 1.U) { // write
      regs(rega) := io.man_di(15,0)
    }*/
  }
  io.man_do := man_do
  io.man_dv := man_dv

  regs( 0) := "b0000_0000_1010_0000".U
  regs( 1) := Cat(Seq("b0000_0000_0010_0".U, io.status, 0.U(2.W)))
  regs( 2) := io.phy_id(31,16) // OUI MSB
  regs( 3) := io.phy_id(15, 0) // OUI LSB, Model/Rev
  regs( 4) := "b0000_0000_0000_0000".U // Empty
  regs( 5) := "b0000_0000_0000_0000".U // Empty
  regs( 6) := "b0000_0000_0000_0000".U // Empty
  regs( 7) := "b0000_0000_0000_0000".U // Empty
  regs( 8) := "b0000_0000_0000_0000".U // Empty
  regs( 9) := "b0000_0000_0000_0000".U // Empty
  regs(10) := "b0000_0000_0000_0000".U // Empty
  regs(11) := "b0000_0000_0000_0000".U // Empty
  regs(12) := "b0000_0000_0000_0000".U // Empty
  regs(13) := "b0000_0000_0000_0000".U // Empty
  regs(14) := "b0000_0000_0000_0000".U // Empty
  regs(15) := "b0000_0001_0000_0000".U // 1000Base-T Full-Duplex
}

class LiteGEMDMA(bundle: TLBundleParameters, queNum: Int, maxReq: Int = 4) extends Module {
  val wReqNum = log2Ceil(maxReq)
  val wQueNum = log2Ceil(queNum)

  val io = IO(new Bundle{
    val dma_do = Vec(queNum, PacketIO(UInt(bundle.dataBits.W)))
    val dma_di = Flipped(Vec(queNum, PacketIO(UInt(bundle.dataBits.W))))
    val dma_ad = Input  (Vec(queNum, UInt(bundle.addressBits.W)))
    val dma_sz = Input  (Vec(queNum, UInt(bundle.sizeBits.W)))
    val dma_we = Input  (Vec(queNum, Bool()))
    val dma_a  = Decoupled(new TLBundleA(bundle))
    val dma_d  = Flipped(Decoupled(new TLBundleD(bundle)))
  })  

  val mem = RegInit(VecInit.fill(maxReq)(0.U(wQueNum.W)))
  val act = RegInit(VecInit.fill(maxReq)(0.B))
  val msk = ~Cat(act.reverse)
  val sop = RegInit(1.B)
  val req = WireDefault(0.U(wReqNum.W))
  val qn0 = WireDefault(0.U(wQueNum.W))
  val qn1 = RegNext(qn0)
  val que = Mux(sop, qn0, qn1)

  qn0 := Log2(Cat(io.dma_di.reverse.map(_.valid)))
  req := Log2(msk)

  when (io.dma_di(que).fire()) {
    sop := io.dma_di(que).last
  }

  when (io.dma_di(que).fire() && sop) {
    mem(req) := que
    act(req) := 1.B
  }

  io.dma_di(que).ready  := msk.orR && io.dma_a.ready
  io.dma_a.valid        := msk.orR && io.dma_di(que).valid
  io.dma_a.bits.data    := io.dma_di(que).data
  io.dma_a.bits.address := io.dma_ad(que)
  io.dma_a.bits.size    := io.dma_sz(que)
  io.dma_a.bits.opcode  := Mux(io.dma_we(que), 0.U, 4.U)
  io.dma_a.bits.param   := 0.U
  io.dma_a.bits.source  := req
  io.dma_a.bits.mask    := 0.U
  io.dma_a.bits.corrupt := 0.B

  //----------------------------------------------------------------------------
  for (idx <- 0 until queNum) {
    io.dma_do(idx).valid := 0.B
    io.dma_do(idx).data  := 0.U
  }
  io.dma_d.ready := 0.B

  val snk = WireDefault(0.U(wQueNum.W))
  when (io.dma_d.valid) {
    snk := mem(io.dma_d.bits.source)
    io.dma_do(snk).valid := io.dma_d.valid
    io.dma_do(snk).data  := io.dma_d.bits.data 
    io.dma_d.ready       := io.dma_do(snk).ready
  }

  //----------------------------------------------------------------------------
  val wDmaDSize = 1 << bundle.sizeBits
  val beatBytes = bundle.dataBits / 8
  val dma_d_cnt = RegInit((beatBytes + 1).U(wDmaDSize.W))
  val dma_d_tot = 1.U((wDmaDSize + 1).W) << io.dma_d.bits.size
  val dma_d_rem = WireDefault(dma_d_tot - dma_d_cnt)
  val dma_d_eop = WireDefault(dma_d_rem(wDmaDSize.U))
  when (io.dma_d.fire()) {
    dma_d_cnt := Mux(dma_d_eop, (beatBytes + 1).U, dma_d_cnt + beatBytes.U)
  }
  io.dma_do(req).last := dma_d_eop

  //----------------------------------------------------------------------------
  when (io.dma_d.fire() && dma_d_eop) {
    act(io.dma_d.bits.source) := 0.B
  }
}
 
class LiteGEMTxD(wData: Int, txqNum: Int = 16, daw64: Bool = 0.B) extends Module {
  val io = IO(new Bundle{
    val gtx_clk = Input (Clock())
    val tx_clk  = Output(Clock())
    val txd     = Output(UInt(8.W))
    val tx_en   = Output(Bool())
    val tx_er   = Output(Bool())

    val tqa     = Input (UInt(64.W))
    val tqe     = Input (Bool())

    val dma_we  = Output(Bool())
    val dma_ad  = Output(UInt(64.W))
    val dma_sz  = Output(UInt(4.W))
    val dma_do  = PacketIO(UInt(wData.W))
    val dma_di  = Flipped(PacketIO(UInt(wData.W))) 
  })

  val wTxDsc = 16

  io.tx_clk := io.gtx_clk
  io.dma_di.ready := 0.B
  io.dma_do.valid := 0.B
  io.dma_do.data  := 0.B
  io.dma_do.last  := 0.B

  val s_idle :: s_req1 :: s_rcv1 :: s_req2 :: s_rcv2 :: s_done :: Nil = Enum(6)
  val state   = RegInit(s_idle)
  val txq_ptr = RegInit(0.U(log2Ceil(txqNum).W))
  val txq_ren = RegInit(0.B)
  val txq_fwd = RegInit(0.B)

  io.dma_ad := 0.U
  io.dma_sz := 0.U
  io.dma_we := 0.B

  val wDscMem = ((wData + 127) / wData) * wData
  val dsc_mem = Reg(UInt(wDscMem.W))
  val dsc_adr = dsc_mem(32, 0)
  val dsc_ctl = dsc_mem(63,32)
  val dsc_ptr = RegInit(0.U(log2Ceil(wDscMem).W))

  val maxReqL = 64 // TL-D Max Burst Size
  val wReqLen = log2Ceil(maxReqL)
  val req_tot = 0.U(1.W) ## dsc_ctl(11, 0)
  val req_ptr = RegInit(1.U(12.W))
  val req_adr = WireDefault(dsc_adr + req_ptr)
  val req_rem = req_tot - req_ptr
  val req_eop = req_rem(12)

  val rsp_sot = RegInit(0.B)
  val rsp_ptr = RegInit(1.U(12.W))
  val rsp_rem = req_tot - rsp_ptr
  val rsp_eop = rsp_rem(12)

  dontTouch(dsc_mem)
  dontTouch(io.dma_we)

  when (io.dma_di.fire()) {
    rsp_sot := io.dma_di.last
  }

  switch (state) {
    is (s_idle) {
      state := Mux(io.tqe, s_req1, s_idle)
    }
    is (s_req1) {
      io.dma_do.valid := 1.B
      io.dma_do.last  := 1.B
      io.dma_do.data  := 0.U
      io.dma_we       := 0.B
      io.dma_sz       := Mux(daw64, 4.U, 3.U)
      io.dma_ad       := io.tqa + txq_ptr
      when (io.dma_do.ready) {
        txq_ptr := txq_ptr + wTxDsc.U
        txq_ren := 1.B
        state   := s_rcv1
        dsc_mem := 0.U
        dsc_ptr := 0.U
      }
    }
    is (s_rcv1) {
      when (io.dma_di.fire() && dsc_ptr =/= wDscMem.U) {
        dsc_mem := dsc_mem | (io.dma_di.data << dsc_ptr)
        dsc_ptr := dsc_ptr + wData.U
      }
      when (io.dma_di.fire() && io.dma_di.last) {
        state   := s_req2
        txq_ren := 0.B
        req_ptr := 0.U
      }
    }
    is (s_req2) {
      io.dma_do.valid := 1.B
      io.dma_do.last  := 1.B
      io.dma_do.data  := 0.U
      io.dma_we       := 0.B
      io.dma_sz       := wReqLen.U
      io.dma_ad       := req_adr
      when (io.dma_do.ready) {
        req_ptr := req_ptr + maxReqL.U
        state   := Mux(req_eop, s_rcv2, s_req2)
        txq_fwd := 1.B
      }
      when (io.dma_di.fire() && io.dma_di.last) {
        rsp_ptr := rsp_ptr + maxReqL.U
      }
    }
    is (s_rcv2) {
      when (io.dma_di.fire() && io.dma_di.last) {
        rsp_ptr := rsp_ptr + maxReqL.U
        when (rsp_eop) {
          txq_fwd := 0.B
          state   := s_done
        }.otherwise {
          state := s_rcv2
        }
      }
    }
    is (s_done) {
      state := s_done
    }
  }

  //----------------------------------------------------------------------------
  val fifo = Module(new PacketQueue(PacketIO(UInt(wData.W)), PacketIO(UInt(8.W))))

  fifo.io.enq.valid := io.dma_di.valid && txq_fwd
  fifo.io.enq.last  := io.dma_di.last && rsp_eop
  fifo.io.enq.data  := io.dma_di.data
  io.dma_di.ready   := txq_ren || txq_fwd && fifo.io.enq.ready

  val txd_val = WireDefault(0.U(8.W))
  val txd_cnt = RegInit(0.U(4.W))
  val txd_ten = fifo.io.deq.valid && fifo.io.cnt.orR 

  fifo.io.deq.ready := 0.B

  when (txd_cnt(3)) {
    fifo.io.deq.ready := 1.B
    txd_val := fifo.io.deq.data
    when (fifo.io.deq.fire() && fifo.io.deq.last) {
      txd_cnt := 0.U
    }
  }.elsewhen (txd_ten) {
    txd_val := Mux(txd_cnt === 7.U, 0x5D.U, 0x55.U)
    txd_cnt := txd_cnt + 1.U
  }

  io.txd   := txd_val
  io.tx_en := txd_ten
  io.tx_er := 0.B
}

