
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
  }
  val reservedMemory = new DeviceSnippet {
    def describe(): Description = {
      Description("reserved-memory", Map(
        "#address-cells" -> Seq(ResourceInt(1)),
        "#size-cells" -> Seq(ResourceInt(1)),
        "ranges" -> Seq[ResourceValue](),
        "opensbi@80000000" -> Seq(ResourceMap(Map(
          "no-map" -> Seq[ResourceValue](),
          "reg" -> Seq(ResourceInt(0x80000000L), ResourceInt(0x200000L))
        )))
      ))
    }
  }*/
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
class WithExtRocketConfig extends Config((site, here, up) => {
  case PeripheryLiteGEMKey  => Seq(LiteGEMParams (0x10014000))
  case PeripheryLiteTTCKey  => Seq(LiteTTCParams (0x10012000))
  case PeripherySPIFlashKey => Seq(SPIFlashParams(0x1001F000, 0x20000000, defaultSampleDel=0))
  case PeripheryUARTKey     => Seq(UARTParams    (0x10010000, initBaudRate = BigInt(115200)))
})

object RocketSystem {
  var modules = Seq[BaseSubsystem]()
  var outputs = Map(
    "RocketSystem" -> {
      () => {
        val config = new Config(
          new WithoutTLMonitors ++
          new WithPeripheryBusFrequency(100) ++
          new WithBootROMFile("bootrom/bootrom-spl.img") ++
          new WithDTS("freechips,rocketchip-riscv64", Nil) ++
          new WithExtMemSize(x"8000_0000") ++
          //new WithInclusiveCache ++
          new WithExtRocketConfig ++
          new DefaultConfig
        )
        val rocket = LazyModule(new RocketSystem()(config))
        modules = modules ++ Seq(rocket)
        rocket.module
      }
    },
    "RocketMed" -> {
      () => {
        val config = new Config(
          new WithoutTLMonitors ++
          new WithPeripheryBusFrequency(100) ++
          new WithBootROMFile("bootrom/bootrom-spl.img") ++
          new WithDTS("freechips,rocketchip-riscv64", Nil) ++
          new WithExtMemSize(x"8000_0000") ++
          new WithExtRocketConfig ++
          new WithNMedCores(1) ++
          new WithCoherentBusTopology ++
          new BaseConfig
        )
        val rocket = LazyModule(new RocketSystem()(config))
        modules = modules ++ Seq(rocket)
        rocket.module
      }
    }
  )
  //----------------------------------------------------------------------------
  def main(args: Array[String]): Unit = {
    var name = if (args.length > 0) args(0) else "RocketSystem" 

    outputs.filter(_._1 == name).foreach(mod => { 
      modules = Seq()
      val outputName = mod._1
      ChiselStage.emitSystemVerilog({
          val module = mod._2()
          module
        },
        firtoolOpts=Array(
          //"--add-vivado-ram-address-conflict-synthesis-bug-workaround",
          "--lowering-options=noAlwaysComb,disallowPackedArrays,disallowLocalVariables",
          //"--preserve-aggregate=1d-vec", // Comment out since iverilog seems dont support 1d-vec = '{...} format
          "--lower-memories",
          "--disable-all-randomization",
          "--disable-annotation-unknown",
          "--strip-debug-info",
          "--verilog", "-o", s"${outputName}.sv" //"--split-verilog", "-o", "out",
        )
      )
      //--------------------------------------------------------------------------
      ElaborationArtefacts.files.foreach {
        case ("dts", contents) => os.write.over(os.pwd / s"${outputName}.dts", contents())
        case _ => ()
      }
      val out = Some(new FileOutputStream((os.pwd / s"${outputName}.dtb").toString))
      modules.head.dtb.contents.foreach(out.get.write(_))
    })
  }
}

