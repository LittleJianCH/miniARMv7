package Memory

import chisel3._

class RAM extends Module {
  val io = IO(new Bundle {
    val wEN = Input(Bool())
    val rEN = Input(Bool())
    val addrR = Input(UInt(32.W))
    val addrW = Input(UInt(32.W))
    val dataR = Output(UInt(32.W))
    val dataW = Input(UInt(32.W))
  })

  val negClock = (~clock.asUInt).asBool.asClock
  val mem = withClock(negClock)(SyncReadMem(2048, UInt(32.W)))
  val wEN = withClock(negClock)(RegNext(io.wEN, false.B))
  val rEN = withClock(negClock)(RegNext(io.rEN, false.B))
  val addrR = withClock(negClock)(RegNext(io.addrR, 0.U))
  val addrW = withClock(negClock)(RegNext(io.addrW, 0.U))
  val dataW = withClock(negClock)(RegNext(io.dataW, 0.U))

  io.dataR := mem.read(addrR(31, 2), rEN, negClock)
  when (wEN) {
    mem.write(addrW(31, 2), dataW, negClock)
  }
}

object RAMGen extends App {
  chisel3.emitVerilog(new RAM, Array("--target-dir", "gen"))
}