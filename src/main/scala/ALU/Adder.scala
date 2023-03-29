package ALU

import chisel3._
import chisel3.util._

class Adder(width: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val cin = Input(UInt(1.W))
    val sum = Output(UInt(width.W))
    val cout = Output(UInt(1.W))
  })

  val p = Cat(io.a & io.b, io.cin)
  val q = io.a ^ io.b

  val g = VecInit((0 to width).map(i =>
    (0 to i).map(j =>
      (j until i).map(k => q(k)).fold(p(j))(_&_)
    ).reduce(_|_)
  )).asUInt

  io.sum := q ^ g(width - 1, 0)
  io.cout := g(width)
}

object AdderGen extends App {
  chisel3.emitVerilog(new Adder(32), Array("--target-dir", "gen"))
}
