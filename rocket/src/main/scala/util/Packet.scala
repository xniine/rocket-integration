
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
  })

  //----------------------------------------------------------------------------
  val wDataOut = dout.data.getWidth
  val wDataIn  = din.data.getWidth

  assert(wDataIn < wDataOut || wDataIn  % wDataOut == 0)
  assert(wDataIn > wDataOut || wDataOut % wDataIn  == 0)
  assert(wDataIn * depth % wDataOut == 0)

  io.deq.last := RegInit(0.B)
  io.deq.data := RegInit(0.U)

  //----------------------------------------------------------------------------
  val buffer = if (wDataIn < wDataOut) {
    SyncReadMem(depth * wDataIn / wDataOut, UInt((wDataOut + 1).W))
  } else {
    SyncReadMem(depth, UInt((wDataIn + 1).W))
  }

  //----------------------------------------------------------------------------
  val wPktP = log2Ceil(count)
  val wMemP = if (wDataIn < wDataOut) {
    log2Ceil(depth * wDataIn / wDataOut)
  } else {
    log2Ceil(depth)
  }

  val pFull = WireDefault(0.B)
  val bHead = RegInit(0.U(wMemP.W))
  val bTail = RegInit(0.U(wMemP.W))
  val bNext = WireDefault(0.U(wMemP.W))

  //----------------------------------------------------------------------------
  // Enque/Deque Packet Metadata
  //----------------------------------------------------------------------------
  if (io.enq.metaType != DontCare) {
    val packet = Reg(Vec(count, io.enq.metaType))
    //--------------------------------------------------------------------------
    val pHead  = Reg (0.U(wPktP.W))
    val pTail  = Reg (0.U(wPktP.W))
    val pNext  = WireDefault(Mux(pHead === (count - 1).U, 0.U, pHead + 1.U))
    //--------------------------------------------------------------------------
    val enqSop = RegInit(1.B)
    when (io.enq.fire()) { enqSop := io.enq.last }
    when (io.enq.fire() && enqSop) {
      packet(pHead) := io.enq.meta
      pHead         := pNext
    }
    //--------------------------------------------------------------------------
    val deqEop = WireDefault(io.deq.fire() && io.deq.last)
    val deqSop = RegInit(0.B)
    when (io.deq.fire()) { deqSop := io.deq.last }
    val deqOut = (deqEop || deqSop && !io.deq.valid) && pHead =/= pTail
    when (deqOut) {
      io.deq.data := packet(pTail)
      pTail       := Mux(pTail === (count - 1).U, 0.U, pTail + 1.U)
    }
    //--------------------------------------------------------------------------
    pFull := (pNext === pTail)
  }

  //----------------------------------------------------------------------------
  // Enque Packet Data
  //----------------------------------------------------------------------------
  if (wDataIn >= wDataOut) {
    val segNum = wDataOut / wDataIn
    bNext := Mux(bHead === (depth - 1).U, 0.U, bHead + 1.U)
    when (io.enq.fire()) {
      buffer.write(bHead, io.enq.last ## io.enq.data.asUInt)
      bHead := bNext
    }
  } else {
    val segNum = wDataOut / wDataIn
    val segCnt = RegInit(0.U(log2Ceil(segNum).W))
    val segBuf = Reg(UInt((wDataOut - wDataIn).W))
    val segEoP = Reg(Bool())

    bNext := Mux(bHead === (depth - 1).U, 0.U, bHead + 1.U)
    bNext := Mux(bHead === (depth - segNum).U, 0.U, bHead + segNum.U)
    when (io.enq.fire()) {
      when (segCnt === (segNum - 1).U || io.enq.last) {
        buffer.write(bHead, (io.enq.last || segEoP) ## io.enq.data.asUInt ## segBuf)
        bHead  := bNext
        segEoP := 0.B
        segBuf := 0.U
      }.otherwise {
        segBuf := io.enq.data.asUInt ## segBuf(wDataIn-1,wDataOut) 
        segEoP := segEoP || io.enq.last
      }
      segCnt := Mux(segCnt === (segNum - 1).U, 0.U, segCnt + 1.U)
    }
  }

  //----------------------------------------------------------------------------
  // Deque Packet Data
  //----------------------------------------------------------------------------
  if (wDataIn <= wDataOut) {
    when (io.deq.fire() || !io.deq.valid && bHead =/= bTail) {
      io.deq.data := buffer.read(bTail)
      io.deq.last := buffer.read(bTail)(wDataOut)
      bTail       := Mux(bTail === (depth - 1).U, 0.U, bTail + 1.U)
    }
  } else {
    val segNum = wDataIn / wDataOut
    val segCnt = RegInit(0.U(log2Ceil(segNum).W))
    val segBuf = Reg(UInt(wDataIn.W))

    val segEoP = Reg(Bool())
    val segReq = (io.deq.ready || !io.deq.valid) && bHead =/= bTail
    val segMem = buffer.read(bTail)

    when (segReq) {
      io.deq.data := segBuf
      io.deq.last := 0.B
      segBuf      := segBuf(wDataIn-1,wDataOut)
      when (segCnt === 0.U) {
        segEoP      := segMem(wDataIn)
        segBuf      := segMem(wDataIn-1,wDataOut)
        io.deq.data := segMem(wDataOut-1,0)
      }.elsewhen (segCnt === (segNum - 1).U) {
        io.deq.last := segEoP
        bTail       := Mux(bTail === (depth - 1).U, 0.U, bTail + 1.U)
      }
      segCnt := Mux(segCnt === (segNum - 1).U, 0.U, segCnt + 1.U)
    }
  }

  //----------------------------------------------------------------------------
  val bFull = WireDefault(bNext === bTail)
  val bWait = WireDefault(bHead =/= bTail)

  io.enq.ready := !(pFull || bFull)
  io.deq.valid := bWait
}


