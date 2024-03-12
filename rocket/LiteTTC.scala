
import chisel3._
import chisel3.util._
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
  val tWidth = 32 // Timer Width
}

////////////////////////////////////////////////////////////////////////////////
class LiteTimerIn extends Bundle {
  val clock_ctl    = UInt(7.W) // CE|CS|PRESCALE[4-1]|EN
  val count_ctl    = UInt(7.W) // PL|OW|CR|ME|DC|IM|OD
  val interval     = UInt(LiteTTCDevice.tWidth.W)
  val match_1      = UInt(LiteTTCDevice.tWidth.W)
  val match_2      = UInt(LiteTTCDevice.tWidth.W)
  val match_3      = UInt(LiteTTCDevice.tWidth.W)
  val interrupt_en = UInt(6.W)
  val interrupt_ac = Bool()
  val event_ctl    = UInt(3.W)
}

class LiteTimerOut extends Bundle {
  val count_out    = UInt(LiteTTCDevice.tWidth.W)
  val event_out    = UInt(LiteTTCDevice.tWidth.W)
  val interrupt_st = UInt(6.W)
}

class LiteTTCIO extends Bundle {
  val ext_clock     = Input (Vec(3, Clock()))
  val waveform      = Output(UInt(3.W))
  val n_waveform_oe = Output(UInt(3.W))
}

class LiteTTC(busWidthBytes: Int, c: LiteTTCParams)(implicit p: Parameters) 
  extends IORegisterRouter(
    RegisterRouterParams(
      name = "ttc",
      compat = Seq("cdns,ttc"),
      base = c.address,
      beatBytes = busWidthBytes),
    new LiteTTCIO)
  with HasInterruptSources
  with HasTLControlRegMap {

  override def extraResources(resources: ResourceBindings) = Map[String, Seq[ResourceValue]](
    "timer-width" -> Seq(ResourceInt(LiteTTCDevice.tWidth)))

  def nInterrupts = 3
  lazy val module = new LazyModuleImp(this) {
    val timer0 = Module(new LiteTimer())
    val timer1 = Module(new LiteTimer())
    val timer2 = Module(new LiteTimer())
    val timers = Vector(timer0, timer1, timer2)

    val output = Reg(Vec(3, new LiteTimerOut))
    val inputs = Reg(Vec(3, new LiteTimerIn))

    //--------------------------------------------------------------------------
    timers.zipWithIndex.foreach { case (tc, i) => 
      when (inputs(i).count_ctl(4)) {
        inputs(i).count_ctl := inputs(i).count_ctl.bitSet(4.U, 0.B)
      }
    }
 
    //--------------------------------------------------------------------------
    val w = LiteTTCDevice.tWidth
    val mapping = Seq(
      0x00 -> Seq(RegField  (7, inputs(0).clock_ctl)),
      0x04 -> Seq(RegField  (7, inputs(1).clock_ctl)),
      0x08 -> Seq(RegField  (7, inputs(2).clock_ctl)),
      0x0C -> Seq(RegField  (7, inputs(0).count_ctl)),
      0x10 -> Seq(RegField  (7, inputs(1).count_ctl)),
      0x14 -> Seq(RegField  (7, inputs(2).count_ctl)),
      0x18 -> Seq(RegField.r(w, output(0).count_out)),
      0x1C -> Seq(RegField.r(w, output(1).count_out)),
      0x20 -> Seq(RegField.r(w, output(2).count_out)),
      0x24 -> Seq(RegField  (w, inputs(0).interval )),
      0x28 -> Seq(RegField  (w, inputs(1).interval )),
      0x2C -> Seq(RegField  (w, inputs(2).interval )),
      0x30 -> Seq(RegField  (w, inputs(0).match_1  )),
      0x34 -> Seq(RegField  (w, inputs(1).match_1  )),
      0x38 -> Seq(RegField  (w, inputs(2).match_1  )),
      0x3C -> Seq(RegField  (w, inputs(0).match_2  )),
      0x40 -> Seq(RegField  (w, inputs(1).match_2  )),
      0x44 -> Seq(RegField  (w, inputs(2).match_2  )),
      0x48 -> Seq(RegField  (w, inputs(0).match_3  )),
      0x4C -> Seq(RegField  (w, inputs(1).match_3  )),
      0x50 -> Seq(RegField  (w, inputs(2).match_3  )),

      0x54 -> Seq(RegField.r(6, RegReadFn(ready => {
        inputs(0).interrupt_ac := 1.B; (1.B, output(0).interrupt_st)
      }))),
      0x58 -> Seq(RegField.r(6, RegReadFn(ready => {
        inputs(1).interrupt_ac := 1.B; (1.B, output(1).interrupt_st)
      }))),
      0x5C -> Seq(RegField.r(6, RegReadFn(ready => {
        inputs(2).interrupt_ac := 1.B; (1.B, output(2).interrupt_st)
      }))),

      0x60 -> Seq(RegField  (6, inputs(0).interrupt_en)),
      0x64 -> Seq(RegField  (6, inputs(1).interrupt_en)),
      0x68 -> Seq(RegField  (6, inputs(2).interrupt_en)),
      0x6C -> Seq(RegField  (3, inputs(0).event_ctl)),
      0x70 -> Seq(RegField  (3, inputs(1).event_ctl)),
      0x74 -> Seq(RegField  (3, inputs(2).event_ctl)),
      0x78 -> Seq(RegField.r(w, output(0).event_out)),
      0x7C -> Seq(RegField.r(w, output(1).event_out)),
      0x80 -> Seq(RegField.r(w, output(2).event_out))
    )
    regmap(mapping:_*)
 
    //--------------------------------------------------------------------------
    timers.zipWithIndex.foreach { case (tc, i) =>
      interrupts(i) := tc.io.interrupt

      when (reset.asBool) {
        inputs(i).clock_ctl    := 0.U
        inputs(i).count_ctl    := "b10000".U
        inputs(i).interval     := 0.U
        inputs(i).match_1      := 0.U
        inputs(i).match_2      := 0.U
        inputs(i).match_3      := 0.U
        inputs(i).interrupt_en := 0.U
        inputs(i).interrupt_ac := 0.B
        inputs(i).event_ctl    := 1.U // Event Disable
      }

      tc.io.ext_clock := port.ext_clock(i)
      tc.io.ctl.in := inputs(i);
      output(i) := tc.io.ctl.out;
    }

    port.waveform      := Cat(timers.map(_.io.waveform))
    port.n_waveform_oe := Cat(timers.map(_.io.n_waveform_oe))
  }
}

////////////////////////////////////////////////////////////////////////////////
class LiteTimerCtlIO extends Bundle {
  val out = Output(new LiteTimerOut)
  val in  = Input (new LiteTimerIn)
}

class LiteTimer extends Module {
  val io = IO(new Bundle {
    val ext_clock     = Input (Clock())
    val interrupt     = Output(Bool())
    val waveform      = Output(Bool())
    val n_waveform_oe = Output(Bool())
    val ctl           = new LiteTimerCtlIO
  })

  val clk_ctl = RegInit(0.U(7.W))
  val cnt_ctl = RegInit(0.U(7.W))
  val itv_num = RegInit(0.U(LiteTTCDevice.tWidth.W))
  val match_1 = RegInit(0.U(LiteTTCDevice.tWidth.W))
  val match_2 = RegInit(0.U(LiteTTCDevice.tWidth.W))
  val match_3 = RegInit(0.U(LiteTTCDevice.tWidth.W))
  val evt_ctl = RegInit(0.U(3.W))

  val evt_out = WireDefault(0.U(LiteTTCDevice.tWidth.W))
  val cnt_out = WireDefault(0.U(LiteTTCDevice.tWidth.W))
  val isr_out = WireDefault(0.U(6.W))

  //----------------------------------------------------------------------------
  val di_hold = RegInit(0.U(4.W))
  di_hold := Mux(di_hold =/= 0.U, di_hold - 1.U, 2.U)
  when (!di_hold) {
    clk_ctl := io.ctl.in.clock_ctl
    cnt_ctl := io.ctl.in.count_ctl
    itv_num := io.ctl.in.interval
    match_1 := io.ctl.in.match_1
    match_2 := io.ctl.in.match_2
    match_3 := io.ctl.in.match_3
    evt_ctl := io.ctl.in.event_ctl
  }

  // CS (Clock Source)
  val clk_src = Mux(clk_ctl(5), io.ext_clock, clock)
  withClock(clk_src) {
    // CDC Input
    val clk_ctl_q1 = RegNext(clk_ctl)
    val clk_ctl_q2 = RegNext(clk_ctl_q1)
    val cnt_ctl_q1 = RegNext(cnt_ctl)
    val cnt_ctl_q2 = RegNext(cnt_ctl_q1)
    val itv_num_q1 = RegNext(itv_num)
    val itv_num_q2 = RegNext(itv_num_q1)
    val match_1_q1 = RegNext(match_1)
    val match_1_q2 = RegNext(match_1_q1)
    val match_2_q1 = RegNext(match_2)
    val match_2_q2 = RegNext(match_2_q1)
    val match_3_q1 = RegNext(match_3)
    val match_3_q2 = RegNext(match_3_q1)
    val evt_ctl_q1 = RegNext(evt_ctl)
    val evt_ctl_q2 = RegNext(evt_ctl_q1)

    //--------------------------------------------------------------------------
    // Clock Prescale
    val div_num = WireDefault(1.U << clk_ctl_q2(4,1)) // Prescale
    val div_cnt = RegInit(1.U(4.W))
    div_cnt := Mux(div_cnt =/= div_num, div_cnt + 1.U, 1.U)
    val clk_pos = WireDefault(div_cnt === div_num)

    //--------------------------------------------------------------------------
    // Overflow, Interval and Counter Reset
    val clk_cnt = RegInit(0.U((LiteTTCDevice.tWidth + 1).W)) 
    val clk_ovf = WireDefault(clk_cnt(LiteTTCDevice.tWidth))
    val clk_itv = WireDefault(0.B)
    val clk_rst = WireDefault(0.B)
    when (cnt_ctl(1) && itv_num_q2.orR && clk_cnt === itv_num_q2) { // IM (Interval Mode)
      clk_itv := 1.B
      clk_rst := 1.B
    }

    //--------------------------------------------------------------------------
    // Counter Value Change
    when (cnt_ctl_q2(4) || clk_rst) { // CR (Counter Reset)
      clk_cnt := 0.U
    }.elsewhen (clk_pos && ~cnt_ctl_q2(0)) { // OD (Output Enable, 1 to freeze)
      val cnt = 0.B ## clk_cnt(LiteTTCDevice.tWidth - 1, 0)
      clk_cnt := Mux(cnt_ctl(2), cnt - 1.U, cnt + 1.U)
    }

    //--------------------------------------------------------------------------
    // ME (Match Enable)
    val clk_mat = Wire(Vec(3, Bool()))
    clk_mat(0) := (cnt_ctl_q2(3) && match_1 === clk_cnt)
    clk_mat(1) := (cnt_ctl_q2(3) && match_2 === clk_cnt)
    clk_mat(2) := (cnt_ctl_q2(3) && match_3 === clk_cnt)

    //----------------------------------------------------------------------------
    // Waveform Output
    val waveform = RegInit(0.B)
    val waveform_oe = RegInit(1.B)

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
    io.n_waveform_oe := waveform_oe
    io.waveform      := waveform

    //----------------------------------------------------------------------------
    // Counter Event
    val evt_act = RegInit(0.B)
    val evt_cnt = RegInit(0.U((LiteTTCDevice.tWidth + 1).W))
    val evt_ovf = WireDefault(evt_cnt(LiteTTCDevice.tWidth))
 
    when (evt_ctl_q2(2)) {
      evt_act := 1.B
    }.elsewhen (!evt_ctl_q2(0)) {
      evt_act := evt_act && !clk_ovf
    }

    when (evt_ctl_q2(0)) {
      evt_cnt := 0.U
    }.elsewhen (evt_act) {
      val cnt = 0.B ## evt_cnt(LiteTTCDevice.tWidth - 1, 0)
      evt_cnt := cnt + (evt_ctl(1) ^ io.ext_clock.asBool)
    }

    //----------------------------------------------------------------------------
    // CDC Output
    val evt_out_q0 = RegInit(0.U(LiteTTCDevice.tWidth.W))
    val cnt_out_q0 = RegInit(0.U(LiteTTCDevice.tWidth.W))
    val isr_out_q0 = RegInit(0.U(6.W))

    val do_hold = RegInit(0.U(4.W))
    do_hold := Mux(do_hold =/= 0.U, do_hold - 1.U, 2.U)
    when (!do_hold) {
      val isr_act = evt_ovf ## clk_ovf ## Cat(clk_mat.reverse)
      evt_out_q0 := evt_cnt
      cnt_out_q0 := clk_cnt
      isr_out_q0 := isr_act ## isr_act.orR
    }

    evt_out := evt_out_q0
    cnt_out := cnt_out_q0
    isr_out := isr_out_q0
  }

  //----------------------------------------------------------------------------
  val evt_out_q1 = RegNext(evt_out)
  val evt_out_q2 = RegNext(evt_out_q1)
  val cnt_out_q1 = RegNext(cnt_out)
  val cnt_out_q2 = RegNext(cnt_out_q1)
  val isr_out_q1 = RegNext(isr_out)
  val isr_out_q2 = RegNext(isr_out_q1)

  val isr_val = RegInit(0.U(6.W))
  when (io.ctl.in.interrupt_ac) {
    isr_val := 0.U
  }.otherwise {
    isr_val := (isr_val | isr_out_q2) & io.ctl.in.interrupt_en 
  }
  io.interrupt := isr_val(0) && !io.ctl.in.interrupt_ac

  io.ctl.out.count_out := cnt_out_q2
  io.ctl.out.event_out := evt_out_q2
  io.ctl.out.interrupt_st := isr_val
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

