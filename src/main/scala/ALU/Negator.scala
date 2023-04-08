package ALU

import chisel3._

class Negator(width: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val out = Output(UInt(width.W))
  })

  val inc = Module(new Inc(width))
  inc.io.in := ~io.in
  io.out := inc.io.out
}
