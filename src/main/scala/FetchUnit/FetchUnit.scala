package FetchUnit

import chisel3._
import chisel3.util._

import ALU.Inc

class FetchUnit(instrs: Seq[String] = Seq()) extends Module {
  // the instrs are represented as binary strings *without* the leading "b"
  val io = IO(new Bundle {
    val nzcv = Input(UInt(4.W))
    val pc = Input(UInt(32.W))
    val pcNext = Output(UInt(32.W))
    val instr = Output(UInt(32.W))
    val cond = Output(Bool())
  })

  val inc4 = Module(new Inc(30))
  inc4.io.in := io.pc(31, 2)
  io.pcNext := Cat(inc4.io.out, io.pc(1, 0))

  val instrROM = VecInit((0 to 63).map(i =>
    if (i < instrs.length) {
      ("b" ++ instrs(i)).U(32.W)
    } else {
      0.U(32.W)
    }
  ))

  io.instr := instrROM(io.pc(7, 2))

  val condCode = io.instr(31, 28)

  val N = io.nzcv(3).asBool
  val Z = io.nzcv(2).asBool
  val C = io.nzcv(1).asBool
  val V = io.nzcv(0).asBool

  io.cond := MuxLookup(condCode, true.B, Seq(
    "b0000".U -> Z,
    "b0001".U -> !Z,
    "b0010".U -> C,
    "b0011".U -> !C,
    "b0100".U -> N,
    "b0101".U -> !N,
    "b0110".U -> V,
    "b0111".U -> !V,
    "b1000".U -> (C && !Z),
    "b1001".U -> (!C || Z),
    "b1010".U -> (N === V),
    "b1011".U -> (N =/= V),
    "b1100".U -> (!Z && (N === V)),
    "b1101".U -> (Z || (N =/= V)),
  ))
}

object FetchUnitGen extends App {
  chisel3.emitVerilog(new FetchUnit, Array("--target-dir", "gen"))
}