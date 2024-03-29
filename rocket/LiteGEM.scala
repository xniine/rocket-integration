
import chisel3._
import chisel3.util._
import chisel3.util.random._

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

  val ethClk = LazyModule(new LiteGEMFixedCLK) 
  eths.foreach { eth =>
    eth.ethClockNode := ethClk.clockNode
  }

  val ethClkNode = ethClk.ioNode.makeSink()
  val ethNodes = eths.map(_.ioNode.makeSink())
  ResourceBinding {
    eths.foreach { eth =>
      Resource(eth.device, "_clocks").bind(ResourceReference(ethClk.device.label))
    }
  }
}

trait HasPeripheryLiteGEMImp extends LazyModuleImp {
  val outer: HasPeripheryLiteGEM
  val eth = outer.ethNodes.zipWithIndex.map { case(n,i) => n.makeIO()(ValName(s"eth_$i")) }
  val ethClock = outer.ethClkNode.makeIOs()(ValName(s"eth_clk"))
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
class LiteGEMFixedCLK(implicit p: Parameters) extends LazyModule {
  val device = new DeviceSnippet {                                                                                
    def describe(): Description = {                                                                                       
      Description("eth_clk", Map(                                                                                 
        "#clock-cells" -> Seq(ResourceInt(0)),
        "clock-output-names" -> Seq(ResourceString("ethernet_clk")),
        "clock-frequency" -> Seq(ResourceInt(125000000L)),
        "compatible" -> Seq(ResourceString("fixed-clock"))
      ))                                                                                                                  
    }
  }
  val clockNode = ClockSourceNode(Seq(ClockSourceParameters(give = Some(ClockParameters(freqMHz = 125)))))
  val ioNode = BundleBridgeSource(() => new Bundle { val clock = Input(Clock()) } )

  lazy val module = new LazyModuleImp(this) {
    clockNode.out.foreach { case (bundle, _) => {
      bundle.clock := ioNode.bundle.clock 
      bundle.reset := 0.B
    }}
  }
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
  val IER    = UInt(32.W) // 0x028, Interrupt Enable
  val IDR    = UInt(32.W) // 0x02C, Interrupt Disable
}

class LiteGEMOut extends Bundle {
  val MID    = UInt(32.W) // 0x0FC, Module ID
                          //        [16+:12] ID NUM (GEM >= 0x2)
  val NSR    = UInt(32.W) // 0x008, Network status
                          //        [ 2] IDLE
  val TSR    = UInt(32.W) // 0x014, Transmit status
                          //        [ 3] TGO - Transmit go
  val ISR    = UInt(32.W) // 0x024, Interrupt Status
                          //        [ 1] RCOMP - Receive Complete
                          //        [ 2] RXUBR - RX used bit read
                          //        [ 3] TXUBR - TX used bit read
                          //        [ 7] TCOMP - Tramsmit Complete
                          //        [10] ROVR  - Receive overrun
  val IMR    = UInt(32.W) // 0x030, Interrupt Mask
}

class LiteGEMIO extends Bundle {
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
    "clocks" -> (resources("clocks") ++ resources("_clocks")).map(_.value),
    "clock-names" -> Seq(ResourceString("hclk"), ResourceString("pclk")),
    "mac-address" -> Seq(0, 0, 0, 0, 0, 0).map(ResourceInt(_)),
    "phy-mode" -> Seq(ResourceString("gmii")),
    "fixed-link" -> Seq(ResourceMap(Map(
      "speed" -> Seq(ResourceInt(1000)),
      "full-duplex" -> Seq()
    )))
  )

  val ethClockNode = ClockSinkNode(Seq(ClockSinkParameters()))
  val dmaClockNode = ClockSinkNode(Seq(ClockSinkParameters()))
  val dmaNode = TLClientNode(Seq(TLMasterPortParameters.v1(
    Seq(TLMasterParameters.v1(
      //supportsProbe = TransferSizes(1, beatBytes),
      requestFifo = true,
      sourceId = IdRange(0, 4),
      name = "dma_port_tl")))))

  def nInterrupts = 1
  lazy val module = new LazyModuleImp(this) {
    val ethClock = ethClockNode.in.head._1.clock
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
    inputs.IDR := 0.U
    inputs.IER := 0.U

    val isr_clr = WireDefault(0.B)
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
      
      0x4C8 -> Seq(RegField  (32, inputs.TBQPH )), // 0x4C8, TX Q Base Address
      0x4D4 -> Seq(RegField  (32, inputs.RBQPH )), // 0x4D4, RX Q Base Address
      0x024 -> Seq(RegField.r(32, RegReadFn(ready => {
        isr_clr := ready; (1.B, output.ISR) 
      }))), // 0x024, Interrupt Status
      0x028 -> Seq(RegField  (32, inputs.IER   )), // 0x028, Interrupt Enable
      0x02C -> Seq(RegField  (32, inputs.IDR   )), // 0x02C, Interrupt Disable
      0x030 -> Seq(RegField.r(32, output.IMR   ))  // 0x030, Interrupt Mask
    )
    regmap(mapping:_*)

    //--------------------------------------------------------------------------
    val eth = Module(new LiteGEMAdapter(beatBytes, dmaEdge.bundle))
    output := eth.io.ctl.out
    eth.io.ctl.in := inputs

    when (eth.io.man_en) {
      inputs.MAN := eth.io.man_do
    }

    when (eth.io.ncr_en) {
      inputs.NCR := eth.io.ncr_do
    }

    eth.io.gtx_clk      := ethClock
    eth.io.port.phy_id  := Mux(port.phy_id.orR, port.phy_id, "h02000000".U)
    eth.io.port.phy_ad  := port.phy_ad
    eth.io.port.status  := port.status

    port.tx_clk         := eth.io.port.tx_clk
    port.tx_en          := eth.io.port.tx_en
    port.tx_er          := eth.io.port.tx_er
    port.txd            := eth.io.port.txd

    eth.io.port.rx_clk  := port.rx_clk
    eth.io.port.rx_er   := port.rx_er
    eth.io.port.rx_dv   := port.rx_dv
    eth.io.port.rxd     := port.rxd

    dma.a <> eth.io.dma_a
    dma.b <> eth.io.dma_b
    dma.c <> eth.io.dma_c
    dma.d <> eth.io.dma_d
    dma.e <> eth.io.dma_e

    //--------------------------------------------------------------------------
    // Interrupts
    val imr_value = (output.IMR | inputs.IER) & ~inputs.IDR
    output.IMR := imr_value

    val isr_value = output.IMR & (output.ISR | eth.io.isr_value)
    output.ISR := Mux(isr_clr, 0.U, isr_value)
    interrupts(0) := output.ISR =/= isr_value && isr_value =/= 0.U
  }
}

////////////////////////////////////////////////////////////////////////////////
class LiteGEMControl extends Bundle {
  val out = Output(new LiteGEMOut)
  val in  = Input (new LiteGEMIn)
}

class LiteGEMAdapter(beatBytes: Int, bundle: TLBundleParameters) extends Module {
  val io = IO(new Bundle {
    val isr_value = Output(UInt(32.W))
    val gtx_clk = Input(Clock())
    val port = new LiteGEMIO
    val ctl = new LiteGEMControl
    val dma_a = Decoupled(new TLBundleA(bundle))
    val dma_b = Flipped(Decoupled(new TLBundleB(bundle)))
    val dma_c = Decoupled(new TLBundleC(bundle))
    val dma_d = Flipped(Decoupled(new TLBundleD(bundle)))
    val dma_e = Decoupled(new TLBundleE(bundle))
    val man_en = Output(Bool())
    val man_do = Output(UInt(32.W))
    val ncr_en = Output(Bool())
    val ncr_do = Output(UInt(32.W))
  })

  io.ctl.out         := 0.U.asTypeOf(io.ctl.out)
  io.ctl.out.MID     := 0x20000.U
  io.ctl.out.NSR     := io.port.status ## 0.U(2.W)
 
  //----------------------------------------------------------------------------
  // Management Port (MDIO)
  //----------------------------------------------------------------------------
  val gem_phy = Module(new LiteGEMPhy)
  gem_phy.io.phy_id  := io.port.phy_id
  gem_phy.io.phy_ad  := io.port.phy_ad
  gem_phy.io.status  := io.port.status
  gem_phy.io.mpe     := io.ctl.in.NCR(4)
  gem_phy.io.man_di  := io.ctl.in.MAN
  io.man_en          := gem_phy.io.man_en
  io.man_do          := gem_phy.io.man_do
 
  //----------------------------------------------------------------------------
  // DMA Mux
  //----------------------------------------------------------------------------
  val gem_dma = Module(new LiteDMA(bundle, 2))
  io.dma_a <> gem_dma.io.dma_a
  io.dma_b <> gem_dma.io.dma_b
  io.dma_c <> gem_dma.io.dma_c
  io.dma_d <> gem_dma.io.dma_d
  io.dma_e <> gem_dma.io.dma_e

  //----------------------------------------------------------------------------
  // Output to NCR
  //----------------------------------------------------------------------------
  val txq_ok = WireDefault(0.B)
  val clr_st = io.ctl.in.NCR(5)

  io.ncr_en := 0.B
  io.ncr_do := 0.U

  when (clr_st) {
    io.ncr_do := io.ctl.in.NCR.bitSet(5.U, 0.B) // CLRSTAT 
    io.ncr_en := 1.B
  }.elsewhen (txq_ok) {
    io.ncr_do := io.ctl.in.NCR.bitSet(9.U, 0.B) // Started, TSTART reset to false
    io.ncr_en := 1.B
  }

  val rx_cmp = WireDefault(0.B)
  val tx_ubr = WireDefault(0.B)

  //----------------------------------------------------------------------------
  // TX Channel
  //----------------------------------------------------------------------------
  withReset(reset.asBool || gem_phy.io.phy_rst || clr_st) {
    val gem_txq = Module(new LiteGEMTxQue(beatBytes * 8))
    gem_txq.io.gtx_clk := io.gtx_clk
    io.port.tx_clk     := gem_txq.io.tx_clk
    io.port.tx_en      := gem_txq.io.tx_en
    io.port.tx_er      := gem_txq.io.tx_er
    io.port.txd        := gem_txq.io.txd

    gem_txq.io.txq_ad  := io.ctl.in.TBQPH ## io.ctl.in.TBQP
    gem_txq.io.txq_en  := io.ctl.in.NCR(3) && io.ctl.in.NCR(9) // TE && TSTART
    txq_ok := gem_txq.io.txq_ok

    gem_dma.io.dma_do(0) <> gem_txq.io.dma_di
    gem_dma.io.dma_di(0) <> gem_txq.io.dma_do
    gem_dma.io.dma_ad(0) := gem_txq.io.dma_ad
    gem_dma.io.dma_sz(0) := gem_txq.io.dma_sz
    gem_dma.io.dma_we(0) := gem_txq.io.dma_we

    tx_ubr := gem_txq.io.tx_ubr
  }

  //----------------------------------------------------------------------------
  // RX Channel
  //----------------------------------------------------------------------------
  withReset(reset.asBool || gem_phy.io.phy_rst || clr_st) {
    val gem_rxq = Module(new LiteGEMRxQue(beatBytes * 8))
    gem_rxq.io.rx_clk := io.port.rx_clk
    gem_rxq.io.rx_dv  := io.port.rx_dv
    gem_rxq.io.rx_er  := io.port.rx_er
    gem_rxq.io.rxd    := io.port.rxd

    gem_rxq.io.rxq_ad := io.ctl.in.RBQPH ## io.ctl.in.RBQP
    gem_rxq.io.rxq_en := io.ctl.in.NCR(2) // RE
    gem_rxq.io.rxq_of := io.ctl.in.NCFGR(15, 14) // RBOF
    
    gem_dma.io.dma_do(1) <> gem_rxq.io.dma_di
    gem_dma.io.dma_di(1) <> gem_rxq.io.dma_do
    gem_dma.io.dma_ad(1) := gem_rxq.io.dma_ad
    gem_dma.io.dma_sz(1) := gem_rxq.io.dma_sz
    gem_dma.io.dma_we(1) := gem_rxq.io.dma_we

    rx_cmp := gem_rxq.io.rx_cmp
  }

  //----------------------------------------------------------------------------
  io.isr_value := tx_ubr ## 0.B ## rx_cmp ## 0.B
}

class LiteGEMPhy extends Module {
  val io = IO(new Bundle {
    val phy_id = Input(UInt(32.W))
    val phy_ad = Input(UInt(5.W))
    val status = Input(Bool())
    val mpe = Input(Bool())
    val man_di = Input(UInt(32.W))
    val man_do = Output(UInt(32.W))
    val man_en = Output(Bool())
    val phy_rst = Output(Bool())
  })
  val man_di = Mux(io.mpe, io.man_di(15, 0), 0.U(16.W))
  val man_do = RegNext(0.U(16.W))
  val man_en = RegNext(0.B)

  val sof    = Mux(io.mpe, io.man_di(31,30), 0.U( 2.W))
  val rw     = Mux(io.mpe, io.man_di(29,28), 0.U( 2.W))
  val phya   = Mux(io.mpe, io.man_di(27,23), 0.U( 5.W))
  val rega   = Mux(io.mpe, io.man_di(22,18), 0.U( 5.W))
  val regs   = Wire(Vec(16, UInt(16.W)))

  io.phy_rst := 0.B
  when (sof === 1.U && phya === io.phy_ad && !rega(4)) {
    when(rw === 2.U) { // read
      man_do := regs(rega)
      man_en := 1.B
    } 
    when (rw === 1.U) { // write
      //regs(rega) := io.man_di(15,0)
      switch(rega) {
        is (0.U) {
          io.phy_rst := io.man_di(15)
        }
      }
    }
  }
  io.man_do := man_do
  io.man_en := man_en

  regs( 0) := "b0000_0001_0100_0000".U
  regs( 1) := Cat(Seq("b0000_0000_0010_0".U, io.status, 0.U(2.W)))
  regs( 2) := io.phy_id(31,16) // OUI MSB
  regs( 3) := io.phy_id(15, 0) // OUI LSB, Model/Rev
  regs( 4) := "b0000_0000_0000_0000".U // Empty
  regs( 5) := "b0000_0000_0000_0000".U // Empty
  regs( 6) := "b0000_0000_0000_0000".U // Empty
  regs( 7) := "b0000_0000_0000_0000".U // Empty
  regs( 8) := "b0000_0000_0000_0000".U // Empty
  regs( 9) := "b0000_0000_0000_0000".U // Empty
  regs(10) := "b0011_1000_0000_0000".U // MII_STAT1000, 1000Base-T Full-Duplex
  regs(11) := "b0000_0000_0000_0000".U // Empty
  regs(12) := "b0000_0000_0000_0000".U // Empty
  regs(13) := "b0000_0000_0000_0000".U // Empty
  regs(14) := "b0000_0000_0000_0000".U // Empty
  regs(15) := "b0000_0000_0000_0000".U // Empty
}

////////////////////////////////////////////////////////////////////////////////
class LiteGEMTxQue(wData: Int, txqNum: Int = 16, daw64: Bool = 0.B) extends Module {
  val io = IO(new Bundle{
    val gtx_clk = Input (Clock())
    val tx_clk  = Output(Clock())
    val txd     = Output(UInt(8.W))
    val tx_en   = Output(Bool())
    val tx_er   = Output(Bool())

    val txq_ad  = Input (UInt(64.W))
    val txq_en  = Input (Bool())
    val txq_ok  = Output(Bool())

    val dma_we  = Output(Bool())
    val dma_ad  = Output(UInt(64.W))
    val dma_sz  = Output(UInt(4.W))
    val dma_do  = PacketIO(UInt(wData.W))
    val dma_di  = Flipped(PacketIO(UInt(wData.W))) 

    val tx_ubr  = Output(Bool())
  })
  io.tx_ubr := 0.B

  val wTxDsc = 16
  val wDataL = log2Ceil(wData)

  io.tx_clk := io.gtx_clk
  io.dma_di.ready := 0.B
  io.dma_do.valid := 0.B
  io.dma_do.data  := 0.B
  io.dma_do.last  := 0.B

  val s_idle :: s_req1 :: s_rcv1 :: s_req2 :: s_rcv2 :: s_req3 :: s_rcv3 :: s_wait :: Nil = Enum(8)
  val state   = RegInit(s_idle)
  val txq_ptr = RegInit(0.U(log2Ceil(txqNum * wTxDsc).W))
  val txq_ren = RegInit(0.B)
  val txq_fwd = RegInit(0.B)

  io.dma_ad := 0.U
  io.dma_sz := 0.U
  io.dma_we := 0.B
  io.txq_ok := 0.B

  val wDscMem = ((wData + 127) / wData) * wData
  val dsc_mem = RegInit(0.U(wDscMem.W))
  val dsc_adr = dsc_mem(32, 0)
  val dsc_ctl = dsc_mem(63,32)
  val dsc_ptr = RegInit(0.U(log2Ceil(wDscMem).W))
  val dsc_nxt = dsc_mem | (io.dma_di.data << dsc_ptr)(63, 0)

  val maxReqL = 64 // TL-D Max Burst Size
  val wReqLen = log2Ceil(maxReqL)
  val pkt_tot = Cat(0.B, dsc_ctl(11, 0)) + dsc_adr(wDataL - 4, 0)
  val pkt_adr = RegInit(0.U(64.W))
  val pkt_gap = RegInit(0.U(4.W))

  val req_ptr = RegInit((maxReqL + 1).U(12.W))
  val rsp_ptr = RegInit((maxReqL + 1).U(12.W))
  val req_rem = pkt_tot - req_ptr
  val rsp_rem = pkt_tot - rsp_ptr
  val req_eop = req_rem(12)
  val rsp_eop = rsp_rem(12)

  switch (state) {
    is (s_idle) {
      state := Mux(io.txq_en, s_req1, s_idle)
    }
    is (s_req1) {
      io.dma_do.valid := 1.B
      io.dma_do.last  := 1.B
      io.dma_do.data  := 0.U
      io.dma_we       := 0.B
      io.dma_sz       := Mux(daw64, 4.U, 3.U)
      io.dma_ad       := io.txq_ad + txq_ptr
      txq_ren         := 1.B
      when (io.dma_do.ready) {
        state   := s_rcv1
        dsc_mem := 0.U
      }
      io.txq_ok := 1.B
    }
    is (s_rcv1) {
      when (io.dma_di.fire && dsc_ptr =/= wDscMem.U) {
        dsc_ptr := dsc_ptr + wData.U
        dsc_mem := dsc_nxt
      }
      when (io.dma_di.fire && io.dma_di.last) {
        pkt_adr := dsc_nxt(95, 64) ## dsc_nxt(31, wDataL - 3) ## 0.U((wDataL - 3).W)
        dsc_ptr := 0.U
        txq_ren := 0.B
        state   := s_req2 //Mux(dsc_nxt(63), s_idle, s_req2) // TX_USED (Bit-31)
        when (dsc_nxt(63)) {
          io.tx_ubr := 1.B
          state := s_idle
        }
      }
    }
    is (s_req2) {
      io.dma_do.valid := 1.B
      io.dma_do.last  := 1.B
      io.dma_do.data  := 0.U
      io.dma_we       := 0.B
      io.dma_sz       := wReqLen.U
      io.dma_ad       := pkt_adr
      txq_fwd         := 1.B
      when (io.dma_do.ready) {
        req_ptr := req_ptr + maxReqL.U
        pkt_adr := pkt_adr + maxReqL.U
        when (req_eop) {
          req_ptr := (maxReqL + 1).U
          state   := s_rcv2
        }
      }
      when (io.dma_di.fire && io.dma_di.last) {
        rsp_ptr := rsp_ptr + maxReqL.U
      }
    }
    is (s_rcv2) {
      when (io.dma_di.fire && io.dma_di.last) {
        rsp_ptr := rsp_ptr + maxReqL.U
        when (rsp_eop) {
          rsp_ptr := (maxReqL + 1).U
          state   := s_req3
          txq_fwd := 0.B
          dsc_mem := dsc_mem.bitSet(63.U, 1.B) // TX_USED (Bit-31)
        }
      }
    }
    is (s_req3) {
      // DMA Req for desc write back 
      io.dma_do.valid := 1.B
      io.dma_do.last  := (wData >= 64).B || (dsc_ptr === 64.U)
      io.dma_do.data  := dsc_mem >> dsc_ptr
      io.dma_we       := 1.B
      io.dma_sz       := 3.U // 64B
      io.dma_ad       := (io.txq_ad + txq_ptr)
      // DMA Req Done
      when (io.dma_do.ready) {
        dsc_ptr := dsc_ptr + wData.U
        when (dsc_ptr === 64.U || (wData >= 64).B) {
          dsc_ptr := 0.U
          state   := s_rcv3
          txq_ren := 1.B
        }
      }
    }
    is (s_rcv3) {
      // DMA Rsp for desc write back
      when (io.dma_di.fire && io.dma_di.last) {
        txq_ptr := Mux(dsc_mem(62), 0.U, txq_ptr + wTxDsc.U) // TX_WRAP (Bit-30)
        txq_ren := 0.B
        state   := s_wait
      }
    }
    is (s_wait) {
      pkt_gap := pkt_gap + 1.U
      // Wait for some time, dont have to be 12CLK, But we use 12 here
      when (pkt_gap === 12.U) {
        pkt_gap := 0.U
        state   := s_req1
      }
    }
  }

  //----------------------------------------------------------------------------
  val din_cnt = RegInit((wData / 8).U(12.W))
  val din_rem = Cat(0.U(1.W), din_cnt) - pkt_tot(11, 0)
  val din_eop = !din_rem(12)
  val din_emp = Mux(din_eop, din_rem, 0.U)
  when (io.dma_di.fire) {
    when (txq_fwd) {
      din_cnt := din_cnt + (wData / 8).U
    }
    when (rsp_eop && io.dma_di.last) {
      din_cnt := (wData / 8).U
    }
  }

  //----------------------------------------------------------------------------
  val fifo = Module(new PacketQueue(PacketIO(UInt(wData.W), emp=true), PacketIO(UInt(8.W), emp=true)))

  fifo.io.enq_shr := dsc_adr
  fifo.io.deq_clk := io.gtx_clk
  fifo.io.enq_clk := clock

  val eth_eop = rsp_eop && dsc_ctl(15) // TX_LAST
  val eth_emp = RegInit(0.B)
  when (io.dma_di.fire) {
    when (io.dma_di.last) {
      eth_emp := 0.B
    }.elsewhen (eth_eop && din_eop) {
      eth_emp := 1.B
    }
  }

  io.dma_di.ready   := txq_fwd && fifo.io.enq.ready || txq_ren || eth_emp
  fifo.io.enq.valid := txq_fwd && io.dma_di.valid && !eth_emp
  fifo.io.enq.empty := din_emp
  fifo.io.enq.last  := eth_eop && din_eop
  fifo.io.enq.data  := io.dma_di.data

  withClock(io.gtx_clk) {
    val txd_val = WireDefault(0.U(8.W))
    val txd_cnt = RegInit(0.U(4.W))
    val txd_ten = fifo.io.deq.valid
    val txd_act = WireDefault(0.B)

    fifo.io.deq.ready := 0.B
    when (txd_cnt(3)) {
      fifo.io.deq.ready := txd_act
      txd_val := fifo.io.deq.data
      when (fifo.io.deq.fire && fifo.io.deq.last) {
        txd_cnt :=  0.U
      }
    }.elsewhen (txd_ten && txd_act) {
      txd_val := Mux(txd_cnt === 7.U, 0xD5.U, 0x55.U)
      txd_cnt := txd_cnt + 1.U
    }

    //--------------------------------------------------------------------------
    // Ethernet FCS
    val txd_fcs = Module(new LiteGEMTxFCS)
    txd_fcs.io.din_tx_en := txd_ten
    txd_fcs.io.din_tx_er := 0.B
    txd_fcs.io.din_txd   := txd_val

    txd_act  := !txd_fcs.io.din_hold
    io.tx_en := txd_fcs.io.out_tx_en
    io.tx_er := txd_fcs.io.out_tx_er
    io.txd   := txd_fcs.io.out_txd
  }
}

class LiteGEMTxFCS extends Module {
  val io = IO(new Bundle {
    val din_tx_en = Input (Bool())
    val din_tx_er = Input (Bool())
    val din_txd   = Input (UInt(8.W))
    val din_hold  = Output(Bool())

    val out_tx_en = Output(Bool())
    val out_tx_er = Output(Bool())
    val out_txd   = Output(UInt(8.W))
  })

  val eth_ten = RegNext(io.din_tx_en)
  val eth_neg = eth_ten && !io.din_tx_en
  val eth_cnt = RegInit(0.U(16.W))
  val pad_cnt = 0x43.U(17.W) - eth_cnt
  val eth_pad = WireDefault(0.B)
  val eth_ipg = WireDefault(0.B)

  val s_idle :: s_data :: s_pads :: s_csum :: s_wait :: Nil = Enum(5)
  val state = RegInit(s_idle)

  val eth_out = WireDefault(0.B)
  val eth_fcs = WireDefault(0.B)
  val fcs_cnt = RegInit(0.U(2.W))
  val ipg_cnt = RegInit(0.U(4.W))

  eth_cnt := Mux(io.din_tx_en && eth_out || eth_pad, eth_cnt + 1.U, 0.U)

  switch (state) {
    is (s_idle) {
      when (io.din_tx_en) {
        state   := s_data
        eth_out := 1.B
      }
    }
    is (s_data) {
      eth_out := 1.B
      when (eth_neg) {
        when (!pad_cnt(16)) {
          state   := s_pads
          eth_pad := 1.B
          eth_out := 0.B
        }.otherwise {
          state   := s_csum
          eth_out := 0.B
          eth_fcs := 1.B
          fcs_cnt := 1.U
        }
      }
    }
    is (s_pads) {
      eth_pad := 1.B
      when (pad_cnt(16)) {
        state   := s_csum
        eth_pad := 0.B
        eth_fcs := 1.B
        fcs_cnt := 1.U
      }
    }
    is (s_csum) {
      eth_fcs := 1.B
      fcs_cnt := fcs_cnt + 1.U
      when (fcs_cnt === 3.U) {
        state   := s_wait
        fcs_cnt := 0.U
      }
    }
    is (s_wait) {
      eth_ipg := 1.B
      ipg_cnt := ipg_cnt + 1.U
      when (ipg_cnt === 11.U) {
        state   := s_idle
        ipg_cnt := 0.U
      }
    }
  }

  io.din_hold  := eth_pad || eth_fcs || eth_ipg
  io.out_tx_en := 0.B
  io.out_tx_er := 0.B
  io.out_txd   := 0.U
  //--------------------------------------------------------------------------
  when (eth_pad) {
    io.out_tx_en := 1.B
    io.out_tx_er := 0.B
    io.out_txd   := 0.U
  }.elsewhen (eth_out) {
    io.out_tx_en := io.din_tx_en
    io.out_tx_er := io.din_tx_er
    io.out_txd   := io.din_txd
  }
  //--------------------------------------------------------------------------
  val lfsr = Module(new GaloisLFSR(32, 
    //taps = Set(26, 23, 22, 16, 12, 11, 10, 8, 7, 5, 4, 2, 1),
    taps = Set(31, 30, 28, 27, 25, 24, 22, 21, 20, 16, 10, 9, 6),
    step = 8,
    seed = Some(BigInt("FFFFFFFF", 16)),
    updateSeed = true 
  ))
  val crc_cnt = eth_cnt(1, 0)
  val crc_din = WireDefault(0.U(32.W))
  val crc_out = Cat(lfsr.io.out.reverse)
  when (eth_out || eth_pad) {
    when (eth_cnt(15, 3) =/= 0.U) {
      crc_din := Mux(eth_pad, crc_out, crc_out ^ io.din_txd)
    }
    when (eth_cnt === 8.U) {
      crc_din := "hFFFFFFFF".U ^ io.din_txd
    }
  }

  lfsr.io.seed.valid := eth_out || eth_pad
  lfsr.io.increment  := 0.B
  for (i <- 0 until 32) {
    lfsr.io.seed.bits(i) := crc_din(i)
  }
  //--------------------------------------------------------------------------
  when (eth_fcs) {
    io.out_tx_en := 1.B
    io.out_tx_er := 0.B
    switch (fcs_cnt) {
      is (0.U) { io.out_txd := ~crc_out( 7,  0) }
      is (1.U) { io.out_txd := ~crc_out(15,  8) }
      is (2.U) { io.out_txd := ~crc_out(23, 16) }
      is (3.U) { io.out_txd := ~crc_out(31, 24) }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
class LiteGEMRxQue(wData: Int, rxqNum: Int = 32, daw64: Bool = 0.B) extends Module {
  val io = IO(new Bundle{
    val rx_clk  = Input (Clock())
    val rxd     = Input (UInt(8.W))
    val rx_dv   = Input (Bool())
    val rx_er   = Input (Bool())

    val rxq_ad  = Input (UInt(64.W))
    val rxq_en  = Input (Bool())
    val rxq_of  = Input (UInt(2.W)) // Receive Buffer Offset

    val dma_we  = Output(Bool())
    val dma_ad  = Output(UInt(64.W))
    val dma_sz  = Output(UInt(4.W))
    val dma_do  = PacketIO(UInt(wData.W))
    val dma_di  = Flipped(PacketIO(UInt(wData.W))) 

    val rx_cmp = Output(Bool())
  })
  io.rx_cmp := 0.B

  val fifo = Module(new PacketQueue(
    PacketIO(UInt(8.W), UInt(17.W), emp=true),
    PacketIO(UInt(wData.W), UInt(17.W), emp=true)))

  fifo.io.enq_shr := 0.U
  fifo.io.enq_clk := io.rx_clk
  fifo.io.deq_clk := clock
 
  withClock(io.rx_clk) {
    val s_idle :: s_init :: s_data :: s_skip :: s_csum :: s_wait :: Nil = Enum(6)
    val rx_mod = RegInit(s_idle)

    fifo.io.enq.valid := 0.B
    fifo.io.enq.data  := 0.U
    fifo.io.enq.last  := 0.B
    fifo.io.enq.empty := 0.U

    val rxq_of_q1 = RegNext(io.rxq_of)
    val rxq_of_q2 = RegNext(rxq_of_q1)

    val buffer = RegInit(0.U(64.W))
    val rx_cnt = RegInit(0.U(16.W))
    val fc_cnt = RegInit(0.U( 8.W))
    val rx_err = WireDefault(0.B)

    fifo.io.enq.meta := Cat(rx_err, rx_cnt)
    when (io.rx_dv) {
       buffer := Cat(io.rxd, buffer(63, 8))
       rx_cnt := Mux(rx_mod === s_idle, 0.U, rx_cnt + 1.U)
    }

    switch (rx_mod) {
      is (s_idle) {
        when (io.rx_dv && io.rxd === 0xd5.U && buffer(63, 8) === "h55555555555555".U) {
          rx_mod := s_init
        }
      }
      is (s_init) {
        when (!io.rx_dv) { 
          rx_mod := s_idle
        }.elsewhen (rx_cnt === 3.U) {
          rx_mod := s_data
        }

        val rxq_of_en = (Cat(1.B, rxq_of_q2) + rx_cnt)
        when (rxq_of_en(3)) {
          fifo.io.enq.valid := 1.B
          fifo.io.enq.last  := 0.B
          fifo.io.enq.data  := 0.U
        }
      }
      is (s_data) {
        fifo.io.enq.valid := 1.B
        fifo.io.enq.data  := buffer(39, 32)
        when (io.rx_dv) {
          when (rx_mod >= 0xFFF.U) {
            fifo.io.enq.valid := 1.B
            fifo.io.enq.last  := 1.B
            rx_mod := s_skip
            rx_err := 1.B
          }
        }.otherwise {
          rx_mod := s_csum
        }
      }
      is (s_skip) {
        fifo.io.enq.valid := 0.B
        fifo.io.enq.last  := 0.B
        fifo.io.enq.data  := 0.U
        when (!io.rx_dv) {
          rx_mod := s_idle
        }
      }
      is (s_csum) {
        val fc_eop = fc_cnt === 16.U
        fifo.io.enq.valid := 1.B
        fifo.io.enq.data  := buffer(63, 40) >> fc_cnt
        fifo.io.enq.last  := fc_eop
        fc_cnt := fc_cnt + 8.U
        when (fifo.io.enq.fire && fc_eop) {
          rx_mod := s_idle
          rx_cnt := 0.U
          fc_cnt := 0.U
        }
      }
    }
  }

  withClock(clock) {
    val wRxDsc = 16

    val s_idle :: s_req1 :: s_rcv1 :: s_req2 :: s_pads :: s_rcv2 :: s_req3 :: s_rcv3 :: Nil = Enum(8)
    val state = RegInit(s_idle)
    val rxq_ptr = RegInit(0.U(log2Ceil(rxqNum * wRxDsc).W))
    val rxq_ren = RegInit(0.B)
    val rxq_fwd = WireDefault(0.B)
 
    val wDscMem = ((wData + 127) / wData) * wData
    val dsc_mem = RegInit(0.U(wDscMem.W))
    val dsc_adr = dsc_mem(32, 0)
    val dsc_ctl = dsc_mem(63,32)
    val dsc_ptr = RegInit(0.U(log2Ceil(wDscMem).W))
    val dsc_nxt = dsc_mem | (io.dma_di.data << dsc_ptr)

    val maxReqL = 64 // TL-D Max Burst Size
    val wReqLen = log2Ceil(maxReqL)
    val pkt_adr = RegInit(0.U(64.W))
    val req_ptr = RegInit(0.U(12.W))
    val rsp_ptr = RegInit(0.U(12.W))
    val frg_ptr = RegInit(0.U(12.W))
    val frg_eop = frg_ptr === (maxReqL - wData / 8).U
    val eth_tot = RegInit(0.U(16.W))

    io.dma_do.valid := 0.B
    io.dma_do.data  := 0.U
    io.dma_do.last  := 0.B
    io.dma_ad       := 0.U
    io.dma_sz       := 0.U
    io.dma_we       := 0.B
 
    switch (state) {
      is (s_idle) {
        state := Mux(io.rxq_en, s_req1, s_idle)
      }
      is (s_req1) {
        io.dma_do.valid := 1.B
        io.dma_do.last  := 1.B
        io.dma_do.data  := 0.U
        io.dma_we       := 0.B
        io.dma_sz       := Mux(daw64, 4.U, 3.U)
        io.dma_ad       := io.rxq_ad + rxq_ptr
        rxq_ren         := 1.B
        when (io.dma_do.ready) {
          state   := s_rcv1
          dsc_mem := 0.U
        }
      }
      is (s_rcv1) {
        when (io.dma_di.fire && dsc_ptr =/= wDscMem.U) {
          dsc_ptr := dsc_ptr + wData.U
          dsc_mem := dsc_nxt //dsc_mem | (io.dma_di.data << dsc_ptr)
        }
        when (io.dma_di.fire && io.dma_di.last) {
          pkt_adr := dsc_nxt(95, 64) ## dsc_nxt(31, 0)
          dsc_ptr := 0.U
          rxq_ren := 0.B
          state   := Mux(dsc_nxt(0), s_idle, s_req2) // RX_USED
        }
      }
      is (s_req2) {
        io.dma_do.valid := fifo.io.deq.valid
        io.dma_do.data  := fifo.io.deq.data
        io.dma_do.last  := frg_eop
        io.dma_we       := 1.B
        io.dma_sz       := wReqLen.U
        io.dma_ad       := pkt_adr
        rxq_ren         := 1.B
        rxq_fwd         := 1.B
        when (io.dma_do.ready) {
          frg_ptr := frg_ptr + (wData / 8).U
          when (frg_eop) {
            pkt_adr := pkt_adr + maxReqL.U
            req_ptr := req_ptr + maxReqL.U
            frg_ptr := 0.U
          }
          when (fifo.io.deq.last) {
            state := Mux(frg_eop, s_rcv2, s_pads)
            eth_tot := fifo.io.deq.meta.asUInt(11, 0)
          }
        }
        when (io.dma_di.fire && io.dma_di.last) {
          rsp_ptr := rsp_ptr + maxReqL.U
        }
      }
      is (s_pads) {
        io.dma_do.valid := 1.B
        io.dma_do.data  := 0.U
        io.dma_do.last  := frg_eop
        io.dma_we       := 1.B
        io.dma_sz       := wReqLen.U
        io.dma_ad       := pkt_adr
        rxq_ren         := 1.B

        when (io.dma_do.ready) {
          frg_ptr := frg_ptr + (wData / 8).U
          when (frg_eop) {
            pkt_adr := pkt_adr + maxReqL.U
            req_ptr := req_ptr + maxReqL.U
            frg_ptr := 0.U
            state := s_rcv2
          }
        }
        when (io.dma_di.fire && io.dma_di.last) {
          rsp_ptr := rsp_ptr + maxReqL.U
        }
      }
      is (s_rcv2) {
        when (io.dma_di.fire && io.dma_di.last) {
          when (rsp_ptr + maxReqL.U === req_ptr) {
            val out_adr = Cat(dsc_mem(31,  1), 1.B)
            val out_ctl = Cat(dsc_mem(63, 44) | 0xC.U, eth_tot(11, 0) - io.rxq_of)
            dsc_mem := Cat(out_ctl, out_adr)
            req_ptr := 0.U
            rsp_ptr := 0.U
            state   := s_req3
          }.otherwise {
            rsp_ptr := rsp_ptr + maxReqL.U
          }
        }
      }
      is (s_req3) {
        // DMA Req for desc write back 
        io.dma_do.valid := 1.B
        io.dma_do.last  := (wData >= 64).B || (dsc_ptr === 64.U)
        io.dma_do.data  := dsc_mem >> dsc_ptr
        io.dma_we       := 1.B
        io.dma_sz       := 3.U // 64B
        io.dma_ad       := io.rxq_ad + rxq_ptr
        // DMA Req Done
        when (io.dma_do.ready) {
          dsc_ptr := dsc_ptr + wData.U
          when (dsc_ptr === 64.U || (wData >= 64).B) {
            dsc_ptr := 0.U
            state   := s_rcv3
            rxq_ren := 1.B
          }
        }
      }
      is (s_rcv3) {
        // DMA Rsp for desc write back
        when (io.dma_di.fire && io.dma_di.last) {
          rxq_ptr   := Mux(dsc_mem(1), 0.U, rxq_ptr + wRxDsc.U) // RX_WRAP
          rxq_ren   := 0.B
          state     := s_req1
          io.rx_cmp := 1.B
        }
      }
    }

    fifo.io.deq.ready := rxq_fwd && io.dma_do.ready
    io.dma_di.ready   := rxq_ren
  }
}
 
