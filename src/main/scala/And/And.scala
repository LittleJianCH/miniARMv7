package And

import chisel3._

class And extends Module {
  val io = IO(new Bundle {
    val in1 = Input(UInt(1.W))
    val in2 = Input(UInt(1.W))
    val out = Output(UInt(1.W))
  })
  io.out := io.in1 & io.in2
}

object Main extends App {
  chisel3.emitVerilog(new And, Array("--target-dir", "gen"))
}
