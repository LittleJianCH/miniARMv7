package ALU

import chisel3._
import chisel3.util._

class HalfAdder extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(1.W))
    val b = Input(UInt(1.W))
    val sum = Output(UInt(1.W))
    val cout = Output(UInt(1.W))
  })

  io.sum := io.a ^ io.b
  io.cout := io.a & io.b
}

class FullAdder extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(1.W))
    val b = Input(UInt(1.W))
    val c = Input(UInt(1.W))
    val sum = Output(UInt(1.W))
    val cout = Output(UInt(1.W))
  })

  val ha1 = Module(new HalfAdder)
  val ha2 = Module(new HalfAdder)

  ha1.io.a := io.a
  ha1.io.b := io.b
  ha2.io.a := io.c
  ha2.io.b := ha1.io.sum

  io.sum := ha2.io.sum
  io.cout := ha1.io.cout | ha2.io.cout
}

class Inc(width: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val out = Output(UInt(width.W))
    val carry = Output(UInt(1.W))
  })

  io.out := VecInit((0 until width).map { i =>
    if (i == 0) !io.in(0)
    else io.in(i - 1, 0).andR ^ io.in(i)
  }).asUInt
  io.carry := io.in.andR
}

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
