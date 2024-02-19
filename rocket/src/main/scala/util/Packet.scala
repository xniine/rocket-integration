
import scala.collection.immutable._
import chisel3.util._
import chisel3._

class PacketIO[T1 <: Data, T2 <: Data](gen: T1, mta: T2 = DontCare, emp: Boolean = false) extends Bundle {
  val valid = Output(new Bool())
  val ready = Input (new Bool())
  val last  = Output(new Bool())
  val data  = Output(gen)
  val meta  = {
    if (mta == DontCare) { new Bundle{ } } else { Output(mta) }
  }
  val empty = {
    if (emp) Output(UInt(log2Ceil(gen.getWidth / 8).W)) else new Bundle{ }
  }

  def metaType = mta
  def hasEmpty = emp
  def fire: Bool = valid && ready
}

object PacketIO {
  def apply[T1 <: Data, T2 <: Data](gen: T1, mta: T2 = DontCare, emp: Boolean = false) = {
    new PacketIO[T1, T2](gen, mta, emp)
  }
}

////////////////////////////////////////////////////////////////////////////////
class PacketQueue[T1 <: Data, T2 <: Data, T3 <: Data] (
  enque: => PacketIO[T2, T1],
  deque: => PacketIO[T3, T1],
  depth: Int = 256,
  count: Int = 64,
  async: Boolean = true,
  frame: Boolean = true
) extends Module {
  val io = IO(new Bundle {
    val enq_clk = Input(Clock())
    val deq_clk = Input(Clock())
    val enq = Flipped(enque)
    val deq = deque
  })
 
  //----------------------------------------------------------------------------
  val wDataIn = enque.data.getWidth
  val wOutput = deque.data.getWidth
  val hasEmpt = enque.hasEmpty

  assert(wDataIn < wOutput || wDataIn  % wOutput == 0)
  assert(wDataIn > wOutput || wOutput % wDataIn  == 0)
  assert(wDataIn * depth % wOutput == 0)
 
  //----------------------------------------------------------------------------
  val wPktP = log2Ceil(count)
  val wMemP = if (wDataIn < wOutput) {
    log2Ceil(depth * wDataIn / wOutput)
  } else {
    log2Ceil(depth)
  }

  //----------------------------------------------------------------------------
  val buffer = if (wDataIn < wOutput) {
    Mem(depth * wDataIn / wOutput, UInt((wOutput + 1).W))
  } else {
    Mem(depth, UInt((wDataIn + 1).W))
  }

  val buf_h2w = WireDefault(0.U((wMemP + 1).W))
  val buf_e2r = WireDefault(0.U((wMemP + 1).W))
  val buf_hq0 = buf_h2w ^ buf_h2w(wMemP, 1)
  val buf_eq0 = buf_e2r ^ buf_e2r(wMemP, 1)

  val buf_ful = WireDefault(0.B)
  val buf_emp = WireDefault(0.B)
  val pkt_ful = WireDefault(0.B)
  val pkt_emp = WireDefault(0.B)

  dontTouch(buf_ful)
  dontTouch(buf_emp)
  dontTouch(pkt_emp)

  val enq_clk = if (async) io.enq_clk else clock
  val deq_clk = if (async) io.deq_clk else clock

  //----------------------------------------------------------------------------
  // Pointers for Packet Full/Empty
  //----------------------------------------------------------------------------
  val pkt_hdr = WireDefault(0.U((wPktP + 1).W)) 
  val pkt_end = WireDefault(0.U((wPktP + 1).W))

  if (io.enq.metaType != DontCare || hasEmpt || frame) {
    val pkt_hq0 = pkt_hdr ^ pkt_hdr(wPktP - 1, 1)
    val pkt_eq0 = pkt_end ^ pkt_end(wPktP - 1, 1)

    //--------------------------------------------------------------------------
    withClock (enq_clk) {
      val pkt_h2w = RegInit(0.U((wPktP + 1).W))
      pkt_hdr := pkt_h2w

      if (async) {
        val pkt_eq1 = RegNext(pkt_eq0)
        val pkt_eq2 = RegNext(pkt_eq1)
        pkt_ful := pkt_hq0 === Cat(~pkt_eq2(wPktP, wPktP - 1), pkt_eq2(wPktP - 2, 0))
      } else {
        pkt_ful := pkt_h2w === Cat(~pkt_end(wPktP), pkt_end(wPktP - 1, 0))
      }

      val enq_eop = WireDefault(io.enq.fire && io.enq.last)
      when (enq_eop) {
        pkt_h2w := Mux(pkt_h2w(wPktP - 1, 0) === count.U, Cat(!pkt_h2w(wPktP), 0.U(wPktP.W)), pkt_h2w + 1.U)
      }
    }
 
    //--------------------------------------------------------------------------
    withClock (deq_clk) {
      val pkt_e2r = RegInit(0.U((wPktP + 1).W))
      pkt_end := pkt_e2r

      if (async) {
        val pkt_hq1 = RegNext(pkt_hq0)
        val pkt_hq2 = RegNext(pkt_hq1)
        pkt_emp := pkt_eq0 === pkt_hq2
      } else {
        pkt_emp := pkt_e2r === pkt_hdr
      }
 
      val deq_eop = WireDefault(io.deq.fire && io.deq.last)
      when (deq_eop) {
        pkt_e2r := Mux(pkt_e2r(wPktP - 1, 0) === (count - 1).U, Cat(!pkt_e2r(wPktP), 0.U(wPktP.W)), pkt_e2r + 1.U)
      }
    }
  }

  //----------------------------------------------------------------------------
  // Enqueue/Dequeue Packet MetaData
  //----------------------------------------------------------------------------
  if (io.enq.metaType != DontCare) {
    val packets = Reg(Vec(count, io.enq.metaType))
 
    withClock (enq_clk) {
      val enq_sop = RegInit(1.B)
      when (io.enq.fire) { enq_sop := io.enq.last }
      when (io.enq.fire && enq_sop) {
        packets(pkt_hdr(wPktP - 1, 0)) := io.enq.meta
      }
    }

    withClock (deq_clk) {
      val deq_eop = WireDefault(io.deq.fire && io.deq.last)
      val deq_sop = RegInit(0.B)
      val deq_out = (deq_eop || deq_sop && !io.deq.valid) && !pkt_emp
      when (io.deq.fire) { deq_sop := io.deq.last }
      when (deq_out) {
        io.deq.meta := packets(pkt_end(wPktP - 1, 0))
      }
    }
  }
 
  //----------------------------------------------------------------------------
  // Empty Flags
  //----------------------------------------------------------------------------
  val wEmpt = log2Ceil(wDataIn.max(wOutput) / 8)
  val emp_out = WireDefault(0.U(wEmpt.W))

  if (hasEmpt) {
    val emp_mem = Mem(count, UInt(wEmpt.W))
    withClock(enq_clk) {
      if (wDataIn >= wOutput) {
        when (io.enq.fire && io.enq.last) {
          emp_mem.write(pkt_hdr, io.enq.empty.asUInt, enq_clk)
        }
      } else {
        val seg_num = wOutput / wDataIn
        val seg_cnt = RegInit((seg_num - 1).U(log2Ceil(seg_num).W))
        when (io.enq.fire) {
          emp_mem.write(pkt_hdr, Cat(seg_cnt, io.enq.empty.asUInt), enq_clk)
          seg_cnt := Mux(seg_cnt === 0.U, (seg_num - 1).U, seg_cnt - 1.U)
        }
      }
    }

    withClock(deq_clk) {
      emp_out := emp_mem.read(pkt_end, deq_clk)
    }
  }
  dontTouch(emp_out)

  //----------------------------------------------------------------------------
  // Enqueue/Dequeue Packet Data Frame
  //----------------------------------------------------------------------------
  withClock (enq_clk) {
    val buf_hdr = RegInit(0.U((wMemP + 1).W))
    buf_h2w := buf_hdr

    if (async) {
      val buf_eq1 = RegNext(buf_eq0, 0.U)
      val buf_eq2 = RegNext(buf_eq1, 0.U)
      buf_ful := buf_hq0 === Cat(~buf_eq2(wMemP, wMemP - 1), buf_eq2(wMemP - 2, 0))
    } else {
      buf_ful := buf_hdr === Cat(~buf_e2r(wMemP, wMemP), buf_e2r(wMemP - 1, 0))
    }

    //----------------------------------------------------------------------------
    io.enq.ready := !(buf_ful || pkt_ful)

    if (wDataIn >= wOutput) {
      val seg_num = wOutput / wDataIn
      when (io.enq.fire) {
        buffer.write(buf_hdr, io.enq.last ## io.enq.data.asUInt, enq_clk)
        buf_hdr := Mux(buf_hdr === (depth - 1).U, Cat(!buf_hdr(wMemP), 0.U(wMemP.W)), buf_hdr + 1.U)
      }
    } else {
      val seg_num = wOutput / wDataIn
      val seg_cnt = RegInit(0.U(log2Ceil(seg_num).W))
      val seg_buf = Reg(UInt((wOutput - wDataIn).W))
      val seg_eop = Reg(Bool())

      when (io.enq.fire) {
        seg_cnt := Mux(seg_cnt === (seg_num - 1).U, 0.U, seg_cnt + 1.U)

        when (seg_cnt === (seg_num - 1).U || io.enq.last) {
          buffer.write(buf_hdr, (io.enq.last || seg_eop) ## io.enq.data.asUInt ## seg_buf, enq_clk)
          buf_hdr := Mux(buf_hdr(wMemP - 1, 0) === (depth - 1).U, Cat(!buf_hdr(wMemP), 0.U(wMemP.W)), buf_hdr + 1.U)
          seg_eop := 0.B
          seg_buf := 0.U
        }.otherwise {
          seg_buf := io.enq.data.asUInt ## seg_buf(wDataIn - 1, wOutput) 
          seg_eop := seg_eop || io.enq.last
        }
      }
    }
  }

  //----------------------------------------------------------------------------
  withClock(deq_clk) {
    val buf_end = RegInit(0.U((wMemP + 1).W))
    buf_e2r := buf_end

    if (async) {
      val buf_hq1 = RegNext(buf_hq0, 0.U)
      val buf_hq2 = RegNext(buf_hq1, 0.U)
      buf_emp := buf_eq0 === buf_hq2
    } else {
      buf_emp := buf_end === buf_h2w
    }
    
    //----------------------------------------------------------------------------
    val seg_emp = RegInit(0.B)

    if (wDataIn <= wOutput) {
      when (io.deq.fire || !io.deq.valid && !buf_emp) {
        val deq_mem = buffer.read(buf_end(wMemP - 1, 0), deq_clk)
        io.deq.data := deq_mem
        io.deq.last := deq_mem(wOutput)
        if (hasEmpt) {
          io.deq.empty := emp_out
        }
        buf_end := Mux(buf_end(wMemP - 1, 0) === (depth - 1).U, Cat(!buf_end(wMemP), 0.U(wMemP.W)), buf_end + 1.U)
      }
    } else {
      val wLane   = log2Ceil(wOutput / 8)
      val seg_num = wDataIn / wOutput
      val seg_cnt = RegInit(0.U(log2Ceil(seg_num).W))
      val seg_buf = RegInit(0.U((wDataIn + 1).W))
      val seg_eop = RegInit(0.B)
      val seg_mem = buffer.read(buf_end(wMemP - 1, 0), deq_clk)

      io.deq.data := Mux(seg_cnt === 0.U, seg_mem(wOutput - 1, 0), seg_buf)
      seg_eop     := Mux(seg_cnt === 0.U, seg_mem(wDataIn), seg_eop)
 
      when (io.deq.fire) {
        seg_buf := Mux(seg_cnt === 0.U, seg_mem >> wOutput, seg_buf >> wOutput)
        seg_cnt := Mux(seg_cnt === (seg_num - 1).U, 0.U, seg_cnt + 1.U)
        when (seg_cnt === (seg_num - 1).U) {
          buf_end := Mux(buf_end === (depth - 1).U, 0.U, buf_end + 1.U)
          seg_emp := 0.B
        }
      }

      if (hasEmpt) {
        io.deq.empty := emp_out
        io.deq.last  := seg_eop && seg_cnt + emp_out(wEmpt - 1, wLane) === (seg_num - 1).U
        seg_emp      := io.deq.fire && io.deq.last
      } else {
        io.deq.last  := seg_eop && seg_cnt === (seg_num - 1).U
      }
    }

    //--------------------------------------------------------------------------
    if (frame) {
      io.deq.valid := !seg_emp && !buf_emp && !pkt_emp
    } else {
      io.deq.valid := !seg_emp && !buf_emp
    }
  }
}

