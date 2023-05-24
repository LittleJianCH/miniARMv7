package ALU

import chisel3._
import scalaz._

import ALU.Negator

class Multiplier(width: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val out = Output(UInt((width * 2).W))
  })

  val adderMatrix = for (i <- 0 until width - 1) yield {
    if (i == 0) Array.fill(width - 1)(Module(new HalfAdder).io)
    else Array.fill(width - 1)(Module(new FullAdder).io)
  }

  for (i <- 0 until width - 1) {
    for (j <- 0 until width - 1) {
      val adder = adderMatrix(i)(j)
      if (i == 0) {
        adder.a := io.a(j + 1) & io.b(0)
        adder.b := io.a(j) & io.b(1)
      } else {
        adder match { // some dirty hack to use .c here
          case adder: Bundle { val c: UInt } =>
            adder.a := (if (j == width - 2) io.a(j + 1) & io.b(i)
                        else adderMatrix(i - 1)(j + 1).sum)
            adder.b := io.a(j) & io.b(i + 1)
            adder.c := adderMatrix(i - 1)(j).cout
        }
      }
    }
  }

  io.out := VecInit((Scalaz.mapAccumLeft[Int, Bool, Bundle { val cout: UInt }]
    ((0 until width * 2).toList)
    (null, (lastAdder, i) =>
      if (i == 0) {
        (null, io.a(0) & io.b(0))
      } else if (i < width) {
        (null, adderMatrix(i - 1)(0).sum(0))
      } else if (i == width) {
        val adder = Module(new HalfAdder).io
        adder.a := adderMatrix(width - 2)(0).cout
        adder.b := adderMatrix(width - 2)(1).sum
        (adder, adder.sum(0))
      } else if (i < width * 2 - 1) {
        val adder = Module(new FullAdder).io
        adder.a := (if (i != width * 2 - 2) adderMatrix(width - 2)(i - width + 1).sum
                    else io.a(width - 1) & io.b(width - 1))
        adder.b := adderMatrix(width - 2)(i - width).cout
        adder.c := lastAdder.cout
        (adder, adder.sum(0))
      } else {
        (null, lastAdder.cout(0))
      })
    )._2).asUInt
}

object MultiplierGen extends App {
  chisel3.emitVerilog(new Multiplier(32), Array("--target-dir", "gen"))
}