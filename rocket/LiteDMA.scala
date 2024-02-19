
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

class LiteDMA(bundle: TLBundleParameters, queNum: Int, maxReq: Int = 4) extends Module {
  val wReqNum = log2Ceil(maxReq)
  val wQueNum = log2Ceil(queNum)

  val io = IO(new Bundle{
    val dma_do = Vec(queNum, PacketIO(UInt(bundle.dataBits.W)))
    val dma_di = Flipped(Vec(queNum, PacketIO(UInt(bundle.dataBits.W))))
    val dma_ad = Input  (Vec(queNum, UInt(bundle.addressBits.W)))
    val dma_sz = Input  (Vec(queNum, UInt(bundle.sizeBits.W)))
    val dma_we = Input  (Vec(queNum, Bool()))
    val dma_a  = Decoupled(new TLBundleA(bundle))
    val dma_b  = Flipped(Decoupled(new TLBundleB(bundle)))
    val dma_c  = Decoupled(new TLBundleC(bundle))
    val dma_d  = Flipped(Decoupled(new TLBundleD(bundle)))
    val dma_e  = Decoupled(new TLBundleE(bundle))
  })  

  io.dma_b := DontCare
  io.dma_c := DontCare
  io.dma_e := DontCare

  //----------------------------------------------------------------------------
  val sop = RegInit(1.B)
  val qn0 = WireDefault(0.U(wQueNum.W))
  val qn1 = RegNext(qn0)
  val que = Mux(sop, qn0, qn1)
  val req = WireDefault(0.U(wReqNum.W))
  val mem = RegInit(VecInit.fill(maxReq)(0.U(wQueNum.W)))
  val act = RegInit(VecInit.fill(maxReq)(0.B))
  val msk = ~Cat(act.reverse)

  when (io.dma_di(que).fire) {
    sop := io.dma_di(que).last
  }
  qn0 := Log2(Cat(io.dma_di.reverse.map(_.valid)))
  req := Log2(msk)

  when (io.dma_di(que).fire && sop) {
    mem(req) := que
    act(req) := 1.B
  }

  //----------------------------------------------------------------------------
  io.dma_di(que).ready  := msk.orR && io.dma_a.ready
  io.dma_a.valid        := msk.orR && io.dma_di(que).valid
  io.dma_a.bits.data    := io.dma_di(que).data
  io.dma_a.bits.address := io.dma_ad(que)
  io.dma_a.bits.size    := io.dma_sz(que)
  io.dma_a.bits.opcode  := Mux(io.dma_we(que), 0.U, 4.U)
  io.dma_a.bits.param   := 0.U
  io.dma_a.bits.source  := req
  io.dma_a.bits.mask    := ~0.U((bundle.dataBits / 8).W)
  io.dma_a.bits.corrupt := 0.B

  //----------------------------------------------------------------------------
  for (idx <- 0 until queNum) {
    io.dma_do(idx).valid := 0.B
    io.dma_do(idx).data  := 0.U
    io.dma_do(idx).last  := 0.U
  }
  io.dma_d.ready := 0.B

  //----------------------------------------------------------------------------
  val wDmaDSize = 1 << bundle.sizeBits
  val beatBytes = bundle.dataBits / 8
  val dma_d_cnt = RegInit((beatBytes + 1).U(wDmaDSize.W))
  val dma_d_tot = 1.U((wDmaDSize + 1).W) << io.dma_d.bits.size
  val dma_d_rem = WireDefault(dma_d_tot - dma_d_cnt)
  val dma_d_eop = WireDefault(dma_d_rem(wDmaDSize.U))
  when (io.dma_d.fire) {
    dma_d_cnt := Mux(dma_d_eop, (beatBytes + 1).U, dma_d_cnt + beatBytes.U)
  }

  //----------------------------------------------------------------------------
  val snk = WireDefault(0.U(wQueNum.W))
  when (io.dma_d.valid) {
    snk := mem(io.dma_d.bits.source)
    io.dma_d.ready       := io.dma_do(snk).ready
    io.dma_do(snk).valid := io.dma_d.valid
    io.dma_do(snk).data  := io.dma_d.bits.data 
    io.dma_do(snk).last  := dma_d_eop | io.dma_d.bits.opcode === 0.U(3.W)
  }

  //----------------------------------------------------------------------------
  when (io.dma_d.fire && dma_d_eop) {
    act(io.dma_d.bits.source) := 0.B
  }
}

