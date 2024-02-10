
import scala.collection.immutable._
import chisel3.util._
import chisel3._


class PacketIO[T1 <: Data, T2 <: Data](gen: T1, mta: T2 = DontCare) extends Bundle {
  val valid = Output(new Bool())
  val ready = Input (new Bool())
  val last  = Output(new Bool())
  val data  = Output(gen)
  val meta  = {
    if (mta == DontCare) { 
      new Bundle{ }
    } else {
      Output(mta)
    }
  }

  def metaType = mta
  def fire(): Bool = valid && ready
}

object PacketIO {
  def apply[T1 <: Data, T2 <: Data](gen: T1, mta: T2) = { new PacketIO[T1, T2](gen, mta) }
  def apply[T1 <: Data](gen: => T1) = { new PacketIO[T1, Element](gen) }
}

class PacketQueue[T1 <: Data, T2 <: Data, T3 <: Data] (
  din: => PacketIO[T2, T1], dout: => PacketIO[T3, T1],
  depth: Int = 256,
  count: Int = 64
) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(din)
    val deq = dout
    val cnt = Output(UInt(log2Ceil(count).W))
  })

  //----------------------------------------------------------------------------
  val pkt_enq = io.enq.fire() && io.enq.last
  val pkt_deq = io.deq.fire() && io.deq.last
  val pkt_cnt = RegInit(0.U(log2Ceil(count).W))
  when (pkt_enq && !pkt_deq) {
    pkt_cnt := pkt_cnt + 1.U
  }.elsewhen (!pkt_enq && pkt_deq) {
    pkt_cnt := pkt_cnt - 1.U
  }
  io.cnt := pkt_cnt

  //----------------------------------------------------------------------------
  val wOutput = dout.data.getWidth
  val wDataIn = din.data.getWidth

  assert(wDataIn < wOutput || wDataIn  % wOutput == 0)
  assert(wDataIn > wOutput || wOutput % wDataIn  == 0)
  assert(wDataIn * depth % wOutput == 0)

  io.deq.last := RegInit(0.B)
  io.deq.data := RegInit(0.U)

  //----------------------------------------------------------------------------
  val buffer = if (wDataIn < wOutput) {
    SyncReadMem(depth * wDataIn / wOutput, UInt((wOutput + 1).W))
  } else {
    SyncReadMem(depth, UInt((wDataIn + 1).W))
  }

  //----------------------------------------------------------------------------
  val wPktP = log2Ceil(count)
  val wMemP = if (wDataIn < wOutput) {
    log2Ceil(depth * wDataIn / wOutput)
  } else {
    log2Ceil(depth)
  }

  val pkt_ful = WireDefault(0.B)
  val buf_hdr = RegInit(0.U(wMemP.W))
  val buf_end = RegInit(0.U(wMemP.W))
  val buf_nxt = WireDefault(0.U(wMemP.W))

  //----------------------------------------------------------------------------
  // Enque/Deque Packet Metadata
  //----------------------------------------------------------------------------
  if (io.enq.metaType != DontCare) {
    val packets = Reg(Vec(count, io.enq.metaType))
    val pkt_hdr = Reg (0.U(wPktP.W))
    val pkt_end = Reg (0.U(wPktP.W))
    val pkt_nxt = WireDefault(Mux(pkt_hdr === (count - 1).U, 0.U, pkt_hdr + 1.U))
    //--------------------------------------------------------------------------
    val enq_sop = RegInit(1.B)
    when (io.enq.fire()) { enq_sop := io.enq.last }
    when (io.enq.fire() && enq_sop) {
      packets(pkt_hdr) := io.enq.meta
      pkt_hdr          := pkt_nxt
    }
    //--------------------------------------------------------------------------
    val deq_eop = WireDefault(io.deq.fire() && io.deq.last)
    val deq_sop = RegInit(0.B)
    when (io.deq.fire()) { deq_sop := io.deq.last }
    val deq_out = (deq_eop || deq_sop && !io.deq.valid) && pkt_hdr =/= pkt_end
    when (deq_out) {
      io.deq.data := packets(pkt_end)
      pkt_end     := Mux(pkt_end === (count - 1).U, 0.U, pkt_end + 1.U)
    }
    //--------------------------------------------------------------------------
    pkt_ful := (pkt_nxt === pkt_end)
  }

  //----------------------------------------------------------------------------
  // Enque Packet Data
  //----------------------------------------------------------------------------
  if (wDataIn >= wOutput) {
    val seg_num = wOutput / wDataIn
    buf_nxt := Mux(buf_hdr === (depth - 1).U, 0.U, buf_hdr + 1.U)
    when (io.enq.fire()) {
      buffer.write(buf_hdr, io.enq.last ## io.enq.data.asUInt)
      buf_hdr := buf_nxt
    }
  } else {
    val seg_num = wOutput / wDataIn
    val seg_cnt = RegInit(0.U(log2Ceil(seg_num).W))
    val seg_buf = Reg(UInt((wOutput - wDataIn).W))
    val seg_eop = Reg(Bool())

    buf_nxt := Mux(buf_hdr === (depth - seg_num).U, 0.U, buf_hdr + seg_num.U)
    when (io.enq.fire()) {
      when (seg_cnt === (seg_num - 1).U || io.enq.last) {
        buffer.write(buf_hdr, (io.enq.last || seg_eop) ## io.enq.data.asUInt ## seg_buf)
        buf_hdr := buf_nxt
        seg_eop := 0.B
        seg_buf := 0.U
      }.otherwise {
        seg_buf := io.enq.data.asUInt ## seg_buf(wDataIn - 1, wOutput) 
        seg_eop := seg_eop || io.enq.last
      }
      seg_cnt := Mux(seg_cnt === (seg_num - 1).U, 0.U, seg_cnt + 1.U)
    }
  }

  //----------------------------------------------------------------------------
  // Deque Packet Data
  //----------------------------------------------------------------------------
  if (wDataIn <= wOutput) {
    when (io.deq.fire() || !io.deq.valid && buf_hdr =/= buf_end) {
      io.deq.data := buffer.read(buf_end)
      io.deq.last := buffer.read(buf_end)(wOutput)
      buf_end     := Mux(buf_end === (depth - 1).U, 0.U, buf_end + 1.U)
    }
  } else {
    val seg_num = wDataIn / wOutput
    val seg_cnt = RegInit(0.U(log2Ceil(seg_num).W))
    val seg_buf = RegInit(0.U(wDataIn.W))
    val seg_eop = RegInit(0.B)
    val seg_ptr = Mux(io.deq.fire(), buf_end + 1.U, buf_end)
    val seg_mem = buffer.read(seg_ptr)

    io.deq.last := seg_eop && (seg_cnt === (seg_num - 1).U)
    io.deq.data := Mux(seg_cnt === 0.U, seg_mem(wOutput - 1, 0), seg_buf)
    seg_buf     := Mux(seg_cnt === 0.U, seg_mem >> wOutput, seg_buf >> wOutput)
    seg_eop     := Mux(seg_cnt === 0.U, seg_mem(wDataIn), seg_eop)

    when (io.deq.fire()) {
      seg_cnt := Mux(seg_cnt === (seg_num - 1).U, 0.U, seg_cnt + 1.U)
      when (seg_cnt === (seg_num - 1).U) {
        buf_end := Mux(buf_end === (depth - 1).U, 0.U, buf_end + 1.U)
      }
    }
  }

  //----------------------------------------------------------------------------
  io.enq.ready := buf_nxt =/= buf_end && !pkt_ful
  io.deq.valid := buf_hdr =/= buf_end
}


