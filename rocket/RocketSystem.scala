
import org.chipsalliance.cde.config.{Config,Parameters}
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.system._
import freechips.rocketchip.util._

import sifive.blocks.devices.timer._
import sifive.blocks.devices.uart._
import sifive.blocks.devices.spi._

import circt.stage.ChiselStage
import chisel3._
import chisel3.util._

import java.io._

////////////////////////////////////////////////////////////////////////////////
class RocketSystem(implicit p: Parameters) extends RocketSubsystem
  with HasPeripheryLiteGEM
  with HasPeripheryLiteTTC
  with HasPeripherySPIFlash
  with HasPeripheryUART
  with HasAsyncExtInterrupts
  with CanHaveMasterAXI4MemPort
  with CanHaveMasterAXI4MMIOPort
  with CanHaveSlaveAXI4Port {

  val bootROM = p(BootROMLocated(location)).map {x: BootROMParams => BootROM.attach(x.copy(hang = 0x10000), this, CBUS) }
  val flash = new FlashDevice(tlQSpiNodes.head.device)

  val aliases = new Device {
    def describe(resources: ResourceBindings): Description = {
      Description("aliases", Map({
        "console" -> Seq(resources("console").head.value)
      }))
    }
  }
  val chosen = new Device {
    def describe(resources: ResourceBindings): Description = {
      Description("chosen", Map({
        "tick-timer" -> Seq(resources("tick-timer").head.value)
      }))
    }
  }
  /* val tickTimer = new DeviceSnippet {
    def describe(): Description = {
      Description("timer", Map({
        "compatible" -> Seq(ResourceString("riscv,timer"))
      }))
    }
  } */
  ResourceBinding {
    //Resource(chosen, "tick-timer").bind(ResourceAlias(tickTimer.label))
    //Resource(chosen, "tick-timer").bind(ResourceAlias(clintOpt.head.device.label))
    Resource(chosen, "tick-timer").bind(ResourceAlias(ttcs.head.device.label))
    Resource(aliases, "console").bind(ResourceAlias(uarts.head.device.label))
    Resource(flash, "reg").bind(ResourceAddress(0))
  }

  override lazy val module = new RocketSubsystemModuleImp(this)
    with HasPeripheryLiteGEMImp
    with HasPeripheryLiteTTCImp
    with HasPeripherySPIFlashModuleImp
    with HasPeripheryUARTModuleImp
    with HasRTCModuleImp
    with HasExtInterruptsModuleImp
    with DontTouch
}

////////////////////////////////////////////////////////////////////////////////
class WithRocketConfig extends Config((site, here, up) => {
  case PeripheryLiteGEMKey  => Seq(LiteGEMParams (0x10014000))
  case PeripheryLiteTTCKey  => Seq(LiteTTCParams (0x10012000))
  case PeripherySPIFlashKey => Seq(SPIFlashParams(0x1001F000, 0x20000000, defaultSampleDel=0))
  case PeripheryUARTKey     => Seq(UARTParams    (0x10010000))
})

object RocketSystem {
  var modules = Seq[BaseSubsystem]()
  def apply() = {
    val config = new Config(
      new WithoutTLMonitors ++
      new WithPeripheryBusFrequency(100) ++
      new WithBootROMFile("bootrom/bootrom-spl.img") ++
      new WithDTS("freechips,rocketchip-riscv64", Nil) ++
      new WithExtMemSize(x"8000_0000") ++
      new WithRocketConfig ++
      new DefaultConfig
    )
    val rocket = LazyModule(new RocketSystem()(config))
    modules = modules ++ Seq(rocket)
    rocket.module
  }
  //----------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    val moduleName = "RocketSystem"

    ChiselStage.emitSystemVerilog({
        this() //new TestHarness()(new DefaultConfig)
      },
      firtoolOpts=Array(
        //"--add-vivado-ram-address-conflict-synthesis-bug-workaround",
        "--lowering-options=disallowLocalVariables",
        "--preserve-aggregate=1d-vec",
        "--lower-memories",
        "--disable-all-randomization",
        "--disable-annotation-unknown",
        "--strip-debug-info",
        "--verilog", "-o", s"${moduleName}.sv" //"--split-verilog", "-o", "out",
      )
    )
    //--------------------------------------------------------------------------
    ElaborationArtefacts.files.foreach {
      case ("dts", contents) => os.write.over(os.pwd / s"${moduleName}.dts", contents())
      case _ => ()
    }
    val out = Some(new FileOutputStream((os.pwd / s"${moduleName}.dtb").toString))
    modules.head.dtb.contents.foreach(out.get.write(_))
  }
}

