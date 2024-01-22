
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

case class LiteTTCParams(
  address: BigInt,
  size: Int = 0x1000)

case object PeripheryLiteTTCKey extends Field[Seq[LiteTTCParams]]

trait HasPeripheryLiteTTC { this: BaseSubsystem =>
  val ttcs = p(PeripheryLiteTTCKey) map { ps =>
    LiteTTCAttachParams(ps, PBUS,
      SynchronousCrossing(),
      SynchronousCrossing()).attachTo(this)
  }
  val ttcNodes = ttcs.map(_.ioNode.makeSink())
}

trait HasPeripheryLiteTTCImp extends LazyModuleImp {
  val outer: HasPeripheryLiteTTC
  val ttc = outer.ttcNodes.zipWithIndex.map { case(n,i) => n.makeIO()(ValName(s"ttc_$i")) }
}

////////////////////////////////////////////////////////////////////////////////
case class LiteTTCAttachParams(
  device: LiteTTCParams,
  controlWhere: TLBusWrapperLocation = PBUS,
  controlXType: ClockCrossingType = NoCrossing,
  intXType: ClockCrossingType = NoCrossing
  ) {
    def attachTo(where: Attachable)(implicit p: Parameters): LiteTTC = where {
      val name = s"ttc_${LiteTTCDevice.nextId()}"
      val tlbus = where.locateTLBusWrapper(controlWhere)
      val ttcClockDomainWrapper = LazyModule(new ClockSinkDomain(take = None))
      val ttc = ttcClockDomainWrapper { LazyModule(new LiteTTC(tlbus.beatBytes, device)) }
      ttc.suggestName(name)

      tlbus.coupleTo(s"device_named_${name}") { bus => 
        controlXType match {
          case _: SynchronousCrossing =>
            tlbus.dtsClk.map(_.bind(ttc.device))
            ttcClockDomainWrapper.clockNode := tlbus.fixedClockNode
          case _: RationalCrossing =>
            ttcClockDomainWrapper.clockNode := tlbus.clockNode
          case _: AsynchronousCrossing =>
            val clockGroup = ClockGroup()
            clockGroup := where.asyncClockGroupsNode
            ttcClockDomainWrapper.clockNode := clockGroup
          case _ => ()
        }

        (ttc.controlXing(controlXType)
          := TLFragmenter(tlbus)
          := bus)
      }

      intXType match {
        case _: SynchronousCrossing  => where.ibus.fromSync     := ttc.intXing(intXType)
        case _: RationalCrossing     => where.ibus.fromRational := ttc.intXing(intXType)
        case _: AsynchronousCrossing => where.ibus.fromAsync    := ttc.intXing(intXType)
        case _ => ()
      }

      ttc
    }
}

object LiteTTCDevice {
  val nextId = { var i = -1; () => { i += 1; i } }
  val width = 32.W
}

////////////////////////////////////////////////////////////////////////////////
class LiteTTCIO(c: LiteTTCParams) extends Bundle {
  val interrupts    = Input (UInt(3.W))
  val waveform      = Input (UInt(3.W))
  val n_waveform_oe = Output(UInt(3.W))
  val ext_clock     = Input (Vec(3, Clock()))
}

class LiteTimerIn extends Bundle {
  val clock_control    = UInt(7.W) // CE|CS|PRESCALE[4-1]|EN
  val count_control    = UInt(7.W) // PL|OW|CR|ME|DC|IM|OD
  val interval         = UInt(LiteTTCDevice.width)
  val match_1          = UInt(LiteTTCDevice.width)
  val match_2          = UInt(LiteTTCDevice.width)
  val match_3          = UInt(LiteTTCDevice.width)
  val interrupt_enable = UInt(6.W)
  val interrupt_rst    = Bool()
}

class LiteTimerOut extends Bundle {
  val count_value      = UInt(LiteTTCDevice.width)
  val interrupt_status = UInt(6.W)
}

class LiteTTC(busWidthBytes: Int, c: LiteTTCParams)(implicit p: Parameters) 
  extends IORegisterRouter(
    RegisterRouterParams(
      name = "ttc",
      compat = Seq("cdns,ttc"),
      base = c.address,
      beatBytes = busWidthBytes),
    new LiteTTCIO(c))
  with HasInterruptSources
  with HasTLControlRegMap {

  def nInterrupts = 3

  lazy val module = new LazyModuleImp(this) {
    val counter0 = Module(new LiteTimer())
    val counter1 = Module(new LiteTimer())
    val counter2 = Module(new LiteTimer())
    val counters = Vector(counter0, counter1, counter2)

    counters(0).io.ext_clock := port.ext_clock(0)
    counters(1).io.ext_clock := port.ext_clock(1)
    counters(2).io.ext_clock := port.ext_clock(2)

    val outputs = Reg(Vec(3, new LiteTimerOut))
    val inputs  = Reg(Vec(3, new LiteTimerIn ))
    withReset(reset) {
      inputs.map(in => {
          in.clock_control    :=  0.U
          in.count_control    := 10.U
          in.interval         :=  0.U
          in.match_1          :=  0.U
          in.match_2          :=  0.U
          in.match_3          :=  0.U
          in.interrupt_enable :=  0.U
          in.interrupt_rst    :=  0.B
      })
    }
    
    val w = LiteTTCDevice.width.get
    val mapping = Seq(
      0x00 -> Seq(RegField  (7, inputs (0).clock_control)),
      0x04 -> Seq(RegField  (7, inputs (1).clock_control)),
      0x08 -> Seq(RegField  (7, inputs (2).clock_control)),
      0x0C -> Seq(RegField  (7, inputs (0).count_control)),
      0x10 -> Seq(RegField  (7, inputs (1).count_control)),
      0x14 -> Seq(RegField  (7, inputs (2).count_control)),
      0x18 -> Seq(RegField.r(w, outputs(0).count_value  )),
      0x1C -> Seq(RegField.r(w, outputs(1).count_value  )),
      0x20 -> Seq(RegField.r(w, outputs(2).count_value  )),
      0x24 -> Seq(RegField  (w, inputs (0).interval     )),
      0x28 -> Seq(RegField  (w, inputs (1).interval     )),
      0x2C -> Seq(RegField  (w, inputs (2).interval     )),
      0x30 -> Seq(RegField  (w, inputs (0).match_1      )),
      0x34 -> Seq(RegField  (w, inputs (1).match_1      )),
      0x38 -> Seq(RegField  (w, inputs (2).match_1      )),
      0x3C -> Seq(RegField  (w, inputs (0).match_2      )),
      0x40 -> Seq(RegField  (w, inputs (1).match_2      )),
      0x44 -> Seq(RegField  (w, inputs (2).match_2      )),
      0x48 -> Seq(RegField  (w, inputs (0).match_3      )),
      0x4C -> Seq(RegField  (w, inputs (1).match_3      )),
      0x50 -> Seq(RegField  (w, inputs (2).match_3      )),
      0x54 -> Seq(RegField  (6, RegReadFn(ready => {
        inputs(0).interrupt_rst := 1.B; (1.B, outputs(0).interrupt_status)
      }), ())),
      0x58 -> Seq(RegField  (6, RegReadFn(ready => {
        inputs(1).interrupt_rst := 1.B; (1.B, outputs(1).interrupt_status)
      }), ())),
      0x5C -> Seq(RegField  (6, RegReadFn(ready => {
        inputs(2).interrupt_rst := 1.B; (1.B, outputs(2).interrupt_status)
      }), ())),
      0x60 -> Seq(RegField  (6, inputs(0).interrupt_enable)),
      0x64 -> Seq(RegField  (6, inputs(1).interrupt_enable)),
      0x68 -> Seq(RegField  (6, inputs(2).interrupt_enable))
    )
 
    counters.zipWithIndex.foreach { case (tc, i) =>
      when (tc.io.reset_ack) {
        tc.io.control.count_control.bitSet(4.U, 0.B)
      }

      tc.io.control.clock_control    := inputs(i).clock_control   ;
      tc.io.control.count_control    := inputs(i).count_control   ;
      tc.io.control.interval         := inputs(i).interval        ;
      tc.io.control.match_1          := inputs(i).match_1         ;
      tc.io.control.match_2          := inputs(i).match_2         ;
      tc.io.control.match_3          := inputs(i).match_3         ;
      tc.io.control.interrupt_enable := inputs(i).interrupt_enable;
      tc.io.control.interrupt_rst    := inputs(i).interrupt_rst   ;

      outputs(i).interrupt_status    := tc.io.control.interrupt_status;
      outputs(i).count_value         := tc.io.control.count_value;
    }
    regmap(mapping:_*)
  }
}

////////////////////////////////////////////////////////////////////////////////
class LiteTimerCtl extends Bundle {
  val clock_control    = Input (UInt( 7.W)) // CE|CS|PRESCALE[4-1]|EN
  val count_control    = Input (UInt( 7.W)) // PL|OW|CR|ME|DC|IM|OD
  val count_value      = Output(UInt(LiteTTCDevice.width))
  val interval         = Input (UInt(LiteTTCDevice.width))
  val match_1          = Input (UInt(LiteTTCDevice.width))
  val match_2          = Input (UInt(LiteTTCDevice.width))
  val match_3          = Input (UInt(LiteTTCDevice.width))
  val interrupt_status = Output(UInt( 6.W))
  val interrupt_enable = Input (UInt( 6.W))
  val interrupt        = Output(Bool())
  val interrupt_rst    = Input (Bool())
}

class LiteTimer extends Module {
  val io = IO(new Bundle {
    val ext_clock     = Input (Clock())
    val control       = new LiteTimerCtl()
    val reset_ack     = Output(Bool())
    val waveform      = Output(Bool())
    val n_waveform_oe = Output(Bool())
  })

  val clk_ctl = RegInit(0.U( 7.W))
  val cnt_ctl = RegInit(0.U( 7.W))
  val inv_num = RegInit(0.U(LiteTTCDevice.width))
  val match_1 = RegInit(0.U(LiteTTCDevice.width))
  val match_2 = RegInit(0.U(LiteTTCDevice.width))
  val match_3 = RegInit(0.U(LiteTTCDevice.width))
  val cnt_val = RegInit(0.U(LiteTTCDevice.width))
  val inp_isr = RegInit(0.U( 6.W))
  val rst_ack = RegInit(0.B)

  //----------------------------------------------------------------------------
  val di_hold = RegInit(0.U(4.W))
  di_hold := Mux(di_hold =/= 0.U, di_hold - 1.U, 4.U(4.W))
  when (!di_hold) {
    clk_ctl := io.control.clock_control
    cnt_ctl := io.control.count_control
    inv_num := io.control.interval
    match_1 := io.control.match_1
    match_2 := io.control.match_2
    match_3 := io.control.match_3
  }

  // CS (Clock Source)
  val clk_src = Mux(clk_ctl(5), io.ext_clock, clock)
  withClock(clk_src) {
    val clk_ctl_q1 = RegNext(clk_ctl)
    val clk_ctl_q2 = RegNext(clk_ctl_q1)
    val cnt_ctl_q1 = RegNext(cnt_ctl)
    val cnt_ctl_q2 = RegNext(cnt_ctl_q1)
    val inv_num_q1 = RegNext(inv_num)
    val inv_num_q2 = RegNext(inv_num_q1)
    val match_1_q1 = RegNext(match_1)
    val match_1_q2 = RegNext(match_1_q1)
    val match_2_q1 = RegNext(match_2)
    val match_2_q2 = RegNext(match_2_q1)
    val match_3_q1 = RegNext(match_3)
    val match_3_q2 = RegNext(match_3_q1)

    //--------------------------------------------------------------------------
    val div_num = WireDefault(1.U << clk_ctl_q2(4,1)) // Prescale
    val div_cnt = RegInit(1.U(4.W))
    div_cnt := Mux(div_cnt =/= div_num, div_cnt + 1.U, 1.U)
    val clk_pos = WireDefault(div_cnt === div_num)
    val clk_cnt = RegInit(0.U(LiteTTCDevice.width + 1)) 

    //--------------------------------------------------------------------------
    val clk_ovf = WireDefault(clk_cnt(LiteTTCDevice.width.get))
    val clk_itv = WireDefault(0.B)
 
    //--------------------------------------------------------------------------
    val clk_rst = WireDefault(0.B)
    when (cnt_ctl(1) && inv_num_q2.orR && clk_cnt === inv_num_q2) { // IM (Interval Mode)
      clk_itv := 1.B
      clk_rst := 1.B
    }

    //--------------------------------------------------------------------------
    when (cnt_ctl_q2(4) || clk_rst) { // CR (Counter Reset)
      clk_cnt := 0.U
    }.elsewhen (clk_pos && ~cnt_ctl_q2(0)) { // OD (Output Enable, 1 to freeze)
      val w = LiteTTCDevice.width.get
      clk_cnt := Mux(cnt_ctl(2), clk_cnt(w-1,0) - 1.U, clk_cnt(w-1,0) + 1.U)
    }

    //--------------------------------------------------------------------------
    // ME (Match Enable)
    val clk_mat = Wire(Vec(3, Bool()))
    clk_mat(0) := (cnt_ctl_q2(3) && match_1 === clk_cnt)
    clk_mat(1) := (cnt_ctl_q2(3) && match_2 === clk_cnt)
    clk_mat(2) := (cnt_ctl_q2(3) && match_3 === clk_cnt)

    //----------------------------------------------------------------------------
    val waveform_oe = RegInit(1.B)
    val waveform = RegInit(0.B)

    when(cnt_ctl_q2(5)) { // OW (Output Wave)
      waveform_oe := 0.B
      when (clk_itv) {
        waveform :=  cnt_ctl_q2(6) // PL (Waveform Polarity)
      }.elsewhen (clk_mat(0)) {
        waveform := ~cnt_ctl_q2(6)
      }
    }.otherwise {
      waveform_oe := 1.B
      waveform    := 0.B
    }

    io.waveform      := waveform
    io.n_waveform_oe := waveform_oe

    //----------------------------------------------------------------------------
    val do_hold = RegInit(0.U( 4.W))
    do_hold := Mux(do_hold =/= 0.U, do_hold - 1.U, 4.U(4.W))
    when (!do_hold) {
      cnt_val := clk_cnt
      rst_ack := cnt_ctl_q2(4)
      inp_isr.bitSet(0.U, clk_ovf || clk_itv || clk_mat.orR)
      inp_isr.bitSet(1.U, clk_mat(0))
      inp_isr.bitSet(2.U, clk_mat(1))
      inp_isr.bitSet(3.U, clk_mat(2))
      inp_isr.bitSet(4.U, clk_ovf   )
    }
  }

  //----------------------------------------------------------------------------
  val cnt_val_q1 = RegNext(cnt_val)
  val cnt_val_q2 = RegNext(cnt_val_q1)
  val inp_isr_q1 = RegNext(inp_isr)
  val inp_isr_q2 = RegNext(inp_isr_q1)
  val rst_ack_q1 = RegNext(rst_ack)
  val rst_ack_q2 = RegNext(rst_ack_q1)
  val isr_val    = RegInit(0.U(6.W))

  io.control.count_value := cnt_val_q2
  io.control.interrupt_status := isr_val
  io.control.interrupt := isr_val(0)
  io.reset_ack := rst_ack_q2

  when (io.control.interrupt_rst) {
    isr_val := 0.U
  }.otherwise {
	isr_val := (isr_val | inp_isr_q2) & io.control.interrupt_enable 
  }
}

////////////////////////////////////////////////////////////////////////////////
/*
class TestLiteTTC(implicit p: Parameters) extends RocketSubsystem
    with HasPeripheryLiteTTC
    with HasAsyncExtInterrupts
    with CanHaveMasterAXI4MemPort
    with CanHaveMasterAXI4MMIOPort
    with CanHaveSlaveAXI4Port
{
  val bootROM = p(BootROMLocated(location)).map {x: BootROMParams => BootROM.attach(x.copy(hang = 0x10000), this, CBUS) }
  override lazy val module = new RocketSubsystemModuleImp(this)
    with HasPeripheryLiteTTCImp
    with HasRTCModuleImp
    with HasExtInterruptsModuleImp
    with DontTouch
}

////////////////////////////////////////////////////////////////////////////////
import org.chipsalliance.cde.config.{Config,Parameters}
import circt.stage.ChiselStage
import chisel3._
import chisel3.util._

class WithRocketConfig extends Config((site, here, up) => {
  case PeripheryTripleTcKey => Seq(TripleTcParams(0x10000000))
})

object LiteTTC {
  def apply() = {
    val config = new Config(
      new WithBootROMFile("./bootrom/bootrom-spl.img") ++
      new WithRocketConfig ++
      new DefaultConfig
    )
    val top = LazyModule(new TestLiteTTC()(config))
    top.module
  }
 
  def main(args: Array[String]): Unit = {
    val moduleName = "LiteTTC"
    ChiselStage.emitSystemVerilog({
        this()
      },
      firtoolOpts=Array(
        //"--add-vivado-ram-address-conflict-synthesis-bug-workaround",
        "--lower-memories",
        "--disable-all-randomization",
        "--disable-annotation-unknown",
        "--strip-debug-info",
        "--verilog", "-o", s"${moduleName}.sv" //"--split-verilog", "-o", "out",
      )
    )
  }
}
*/

