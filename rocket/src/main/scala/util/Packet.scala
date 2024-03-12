
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

  assert(wDataIn < wOutput || wDataIn % wOutput == 0)
  assert(wDataIn > wOutput || wOutput % wDataIn == 0)
 
  //----------------------------------------------------------------------------
  val bSize = depth
  val wPktP = log2Ceil(count)
  val wMemP = log2Ceil(bSize)

  //----------------------------------------------------------------------------
  val buffer = if (wDataIn < wOutput) {
    Mem(depth, UInt((wOutput + 1).W))
  } else {
    Mem(depth, UInt((wDataIn + 1).W))
  }

  val buf_hdr = WireDefault(0.U((wMemP + 1).W))
  val buf_end = WireDefault(0.U((wMemP + 1).W))
  val buf_hq0 = buf_hdr ^ buf_hdr(wMemP, 1)
  val buf_eq0 = buf_end ^ buf_end(wMemP, 1)

  val buf_ful = WireDefault(0.B)
  val buf_emp = WireDefault(0.B)
  val pkt_ful = WireDefault(0.B)
  val pkt_emp = WireDefault(0.B)

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
      val enq_sop = RegInit(1.B)
      enq_sop := Mux(enq_eop, 1.B, enq_sop && !io.enq.fire)
      when (io.enq.fire && enq_eop) {
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
    val packets = Mem(count, io.enq.metaType.asUInt)
 
    withClock (enq_clk) {
      val enq_sop = RegInit(1.B)
      when (io.enq.fire) { enq_sop := io.enq.last }
      when (io.enq.fire) {
        packets.write(pkt_hdr(wPktP - 1, 0), io.enq.meta.asUInt, enq_clk)
      }
    }

    withClock (deq_clk) {
      val deq_eop = WireDefault(io.deq.fire && io.deq.last)
      val deq_sop = RegInit(1.B)
      io.deq.meta := Mux(io.deq.valid, packets.read(pkt_end(wPktP - 1, 0), deq_clk), 0.U)
      when (io.deq.fire) { deq_sop := io.deq.last }
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
        val emp_cnt = RegInit((seg_num - 1).U(log2Ceil(seg_num).W))
        when (io.enq.fire) {
          emp_mem.write(pkt_hdr, Cat(emp_cnt, io.enq.empty.asUInt), enq_clk)
          emp_cnt := Mux(emp_cnt === 0.U || io.enq.last, (seg_num - 1).U, emp_cnt - 1.U)
        }
      }
    }

    withClock(deq_clk) {
      emp_out := emp_mem.read(pkt_end, deq_clk)
    }
  }

  //----------------------------------------------------------------------------
  // Enqueue/Dequeue Packet Data Frame
  //----------------------------------------------------------------------------
  withClock (enq_clk) {
    val buf_h2w = RegInit(0.U((wMemP + 1).W))
    buf_hdr := buf_h2w

    if (async) {
      val buf_eq1 = RegNext(buf_eq0, 0.U)
      val buf_eq2 = RegNext(buf_eq1, 0.U)
      buf_ful := buf_hq0 === Cat(~buf_eq2(wMemP, wMemP - 1), buf_eq2(wMemP - 2, 0))
    } else {
      buf_ful := buf_h2w === Cat(~buf_end(wMemP, wMemP), buf_end(wMemP - 1, 0))
    }

    //----------------------------------------------------------------------------
    io.enq.ready := !(buf_ful || pkt_ful)

    if (wDataIn >= wOutput) {
      val seg_num = wOutput / wDataIn
      when (io.enq.fire) {
        buffer.write(buf_h2w, io.enq.last ## io.enq.data.asUInt, enq_clk)
        buf_h2w := Mux(buf_h2w(wMemP - 1, 0) === (bSize - 1).U, Cat(!buf_h2w(wMemP), 0.U(wMemP.W)), buf_h2w + 1.U)
      }
    } else {
      val seg_num = wOutput / wDataIn
      val seg_cnt = RegInit(0.U(log2Ceil(seg_num).W))
      val seg_buf = RegInit(0.U((wOutput - wDataIn).W))
      val seg_eop = RegInit(0.B)

      when (io.enq.fire) {
        seg_cnt := Mux(io.enq.last || seg_cnt === (seg_num - 1).U, 0.U, seg_cnt + 1.U)

        when (seg_cnt === (seg_num - 1).U || io.enq.last) {
          val seg_val = io.enq.data.asUInt ## seg_buf
          buffer.write(buf_h2w, (io.enq.last || seg_eop) ## seg_val, enq_clk)
          buf_h2w := Mux(buf_h2w(wMemP - 1, 0) === (bSize - 1).U, Cat(!buf_h2w(wMemP), 0.U(wMemP.W)), buf_h2w + 1.U)
          seg_eop := 0.B
          seg_buf := 0.U
        }.otherwise {
          seg_buf := io.enq.data.asUInt ## seg_buf(wOutput - wDataIn - 1, wDataIn) 
          seg_eop := seg_eop || io.enq.last
        }
      }
    }
  }

  //----------------------------------------------------------------------------
  withClock(deq_clk) {
    val buf_e2r = RegInit(0.U((wMemP + 1).W))
    buf_end := buf_e2r

    if (async) {
      val buf_hq1 = RegNext(buf_hq0, 0.U)
      val buf_hq2 = RegNext(buf_hq1, 0.U)
      buf_emp := buf_eq0 === buf_hq2
    } else {
      buf_emp := buf_e2r === buf_hdr
    }
    
    //----------------------------------------------------------------------------
    if (wDataIn <= wOutput) {
      io.deq.data := 0.U
      io.deq.last := 0.B
      if (hasEmpt) {
        io.deq.empty := 0.U
      }

      val deq_mem = buffer.read(buf_e2r(wMemP - 1, 0), deq_clk)
      val deq_val = deq_mem(wOutput - 1, 0)
      val deq_eop = deq_mem(wOutput)

      io.deq.last := deq_eop
      io.deq.data := deq_val
      if (hasEmpt) {
        io.deq.data  := Mux(deq_eop, deq_val >> emp_out ## 0.U(3.W), deq_val)
        io.deq.empty := emp_out
      }

      when (io.deq.fire) {
        buf_e2r := Mux(buf_e2r(wMemP - 1, 0) === (bSize - 1).U, Cat(!buf_e2r(wMemP), 0.U(wMemP.W)), buf_e2r + 1.U)
      }
    } else {
      val wStrb   = log2Ceil(wOutput / 8)
      val seg_num = wDataIn / wOutput
      val seg_cnt = RegInit(0.U(log2Ceil(seg_num).W))
      val seg_buf = RegInit(0.U(wDataIn.W))
      val seg_mem = buffer.read(buf_e2r(wMemP - 1, 0), deq_clk)
      val seg_eop = WireDefault(0.B)
      val seg_ep1 = RegNext(seg_eop)
      val seg_eot = WireDefault(0.B)

      io.deq.data := Mux(seg_cnt === 0.U, seg_mem(wOutput - 1, 0), seg_buf)
      seg_eop     := Mux(seg_cnt === 0.U, seg_mem(wDataIn), seg_ep1)

      when (io.deq.ready || !io.deq.valid) {
        if (hasEmpt) {
          val emp = (seg_cnt + emp_out(wEmpt - 1, wStrb) === (seg_num - 1).U)
          seg_eot := seg_cnt === (seg_num - 1).U || seg_eop && emp
        } else {
          seg_eot := seg_cnt === (seg_num - 1).U
        }
      }

      if (hasEmpt) {
        io.deq.empty := Mux(seg_eop, 0.U, emp_out)
        io.deq.last  := seg_eop && seg_eot
      } else {
        io.deq.last  := seg_eop && seg_cnt === (seg_num - 1).U
      }

      when (io.deq.fire) {
        seg_buf := Mux(seg_cnt === 0.U, seg_mem >> wOutput, seg_buf >> wOutput)
        seg_cnt := seg_cnt + 1.U
        when (seg_eot) {
          buf_e2r := Mux(buf_e2r(wMemP - 1, 0) === (bSize - 1).U, Cat(!buf_e2r(wMemP), 0.U(wMemP.W)), buf_e2r + 1.U)
          seg_cnt := 0.U
        }
      }
    }

    //--------------------------------------------------------------------------
    if (frame) {
      io.deq.valid := !buf_emp && !pkt_emp
    } else {
      io.deq.valid := !buf_emp
    }
  }
}

