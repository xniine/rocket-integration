
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

class LiteDMA(bundle: TLBundleParameters, queNum: Int, maxReq: Int = 4, maxLen: Int = 128) extends Module {
  val wReqNum = log2Ceil(maxReq)
  val wQueNum = log2Ceil(queNum)
  val wBstCnt = log2Ceil(maxLen * 8 / bundle.dataBits)

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
  val req_sop = RegInit(1.B)
  val req_qn0 = WireDefault(0.U(wQueNum.W))
  val req_qn1 = RegNext(req_qn0)
  val req_que = Mux(req_sop, req_qn0, req_qn1)

  val req_idx = RegInit(VecInit.fill(maxReq)(0.U(wQueNum.W)))
  val req_act = RegInit(VecInit.fill(maxReq)(0.B))
  val req_msk = ~Cat(req_act.reverse)

  val req_tg0 = WireDefault(0.U(wReqNum.W))
  val req_tg1 = RegInit(0.U(wReqNum.W))
  val req_tag = WireDefault(req_tg1)

  val req_fw1 = RegInit(1.B)
  val req_fwd = WireDefault(req_fw1)

  req_qn0 := Log2(~scanLeftOr(Cat(io.dma_di.reverse.map(_.valid))) + 1.U)
  req_tg0 := Log2(~scanLeftOr(req_msk) + 1.U)

  when (io.dma_di(req_que).fire) {
    req_sop := io.dma_di(req_que).last
  }
  when (req_sop) {
    req_fwd := req_msk.orR
    req_fw1 := req_msk.orR
  }

  when (req_sop) {
    when (io.dma_di(req_que).fire) {
      req_idx(req_tg0) := req_que
      req_act(req_tg0) := 1.B
    }
    req_tg1 := req_tg0
    req_tag := req_tg0
  }
  dontTouch(req_tg0)

  //----------------------------------------------------------------------------
  for (idx <- 0 until queNum) {
    io.dma_do(idx).valid := 0.B
    io.dma_do(idx).data  := 0.U
    io.dma_do(idx).last  := 0.U
    io.dma_di(idx).ready := 0.U
  }
  io.dma_d.ready := 0.B

  //----------------------------------------------------------------------------
  io.dma_di(req_que).ready := req_fwd && io.dma_a.ready
  io.dma_a.valid        := req_fwd && io.dma_di(req_que).valid
  io.dma_a.bits.data    := io.dma_di(req_que).data
  io.dma_a.bits.address := io.dma_ad(req_que)
  io.dma_a.bits.size    := io.dma_sz(req_que)
  io.dma_a.bits.opcode  := Mux(io.dma_we(req_que), 0.U, 4.U)
  io.dma_a.bits.param   := 0.U
  io.dma_a.bits.source  := req_tag
  io.dma_a.bits.mask    := ~0.U((bundle.dataBits / 8).W)
  io.dma_a.bits.corrupt := 0.B

  //----------------------------------------------------------------------------
  val wDmaDSize = 1 << bundle.sizeBits
  val beatBytes = bundle.dataBits / 8
  val dma_d_cnt = RegInit((beatBytes + 1).U(wDmaDSize.W))
  val dma_d_tot = 1.U((wDmaDSize + 1).W) << io.dma_d.bits.size
  val dma_d_rem = WireDefault(dma_d_tot - dma_d_cnt)
  val dma_d_eop = WireDefault(dma_d_rem(wDmaDSize.U))
  val dma_d_ack = io.dma_d.bits.opcode === 0.U
  when (io.dma_d.fire) {
    dma_d_cnt := Mux(dma_d_eop, (beatBytes + 1).U, dma_d_cnt + beatBytes.U)
  }
  dontTouch(dma_d_eop)

  //----------------------------------------------------------------------------
  val rsp_fwd = WireDefault(0.B)
  when (rsp_fwd && io.dma_d.valid) {
    val snk = req_idx(io.dma_d.bits.source)
    io.dma_do(snk).valid := io.dma_d.valid
    io.dma_do(snk).data  := io.dma_d.bits.data 
    io.dma_do(snk).last  := dma_d_eop || dma_d_ack
    io.dma_d.ready       := io.dma_do(snk).ready
  }

  //----------------------------------------------------------------------------
  when (rsp_fwd && io.dma_d.fire && (dma_d_eop || dma_d_ack)) {
    req_act(io.dma_d.bits.source) := 0.B
  }

  //////////////////////////////////////////////////////////////////////////////
  /* Packet Reordering for TL-D */
  //////////////////////////////////////////////////////////////////////////////
  val rsp_rdy = RegInit(VecInit.fill(maxReq)(0.B))
  val rsp_tag = RegInit(VecInit.fill(maxReq)(0.U(wReqNum.W)))
  val rsp_cnt = RegInit(VecInit.fill(maxReq)(0.U(wBstCnt.W)))
  val rsp_ack = RegInit(VecInit.fill(maxReq)(0.B))

  val rsp_ptr = RegInit(0.U(wReqNum.W))
  val rsp_hdr = RegInit(0.U(wReqNum.W))
  val rsp_mem = SyncReadMem(maxReq * maxLen / beatBytes, UInt(bundle.dataBits.W))

  // Save the source-id of TL-A
  when (io.dma_di(req_que).fire && io.dma_di(req_que).last) {
    val req_cnt = 1.U << (io.dma_a.bits.size - log2Ceil(bundle.dataBits / 8).U)
    rsp_tag(rsp_ptr) := io.dma_a.bits.source
    rsp_ack(rsp_ptr) := io.dma_a.bits.opcode =/= 4.U
    rsp_cnt(rsp_ptr) := req_cnt
    rsp_ptr := Mux(rsp_ptr === (maxReq - 1).U, 0.U, rsp_ptr + 1.U)
  }

  val rsp_fit = RegInit(0.B)
  when (io.dma_d.valid) { // Compare source-id of TL-D for immediate forward
    val fit = io.dma_d.bits.source === rsp_tag(rsp_hdr)
    rsp_fit := Mux(io.dma_d.ready && dma_d_eop, 0.B, fit)
    rsp_fwd := fit 
  }.otherwise {
    rsp_fwd := rsp_fit
  }

  //----------------------------------------------------------------------------
  val rsp_src = rsp_tag(rsp_hdr)
  val rsp_que = req_idx(rsp_src)

  when (io.dma_do(rsp_que).fire && io.dma_do(rsp_que).last) {
    rsp_hdr := Mux(rsp_hdr === (maxReq - 1).U, 0.U, rsp_hdr + 1.U)
  }

  // Save out-of-order to packet cache
  val out_ptr = RegInit(0.U(wBstCnt.W))
  when (!rsp_fwd && io.dma_d.valid) {
    val src = io.dma_d.bits.source
    val ptr = Cat(src, out_ptr)
    io.dma_d.ready := 1.B
    rsp_mem(ptr)   := io.dma_d.bits.data
    out_ptr        := Mux(dma_d_eop, 0.U, out_ptr + 1.U)
    when (dma_d_eop || dma_d_ack) {
      rsp_rdy(src) := 1.B
    }
  }

  // Output from cached packet data
  val out_hdr = RegInit(0.U(wBstCnt.W))
  when (!rsp_fwd && rsp_rdy(rsp_src)) {
    when (rsp_ack(rsp_hdr)) {
      io.dma_do(rsp_que).valid := 1.B
      io.dma_do(rsp_que).last  := 1.B
      io.dma_do(rsp_que).data  := 0.U
      when (io.dma_do(rsp_que).ready) {
        req_act(rsp_src) := 0.B
        rsp_rdy(rsp_src) := 0.B
      }
    }.otherwise {
      val out_eop = (rsp_cnt(rsp_hdr) === out_hdr)

      io.dma_do(rsp_que).valid := out_hdr =/= 0.U
      io.dma_do(rsp_que).last  := out_eop
      io.dma_do(rsp_que).data  := Mux(!out_hdr, 0.U, rsp_mem.read(Cat(rsp_src, out_hdr)))

      out_hdr := Mux(!out_hdr, 1.U, out_hdr)
      when (io.dma_do(rsp_que).fire) {
        when (out_eop) {
          req_act(rsp_src) := 0.B
          rsp_rdy(rsp_src) := 0.B
          out_hdr := 0.U
        }.otherwise {
          out_hdr := out_hdr + 1.U
        }
      }
    }
  }
}

