package FetchUnit

import chisel3._
import chisel3.util._

import ALU.Inc

class FetchUnit extends Module {
  val io = IO(new Bundle {
    val nzcv = Input(UInt(4.W))
    val writeEN = Input(Bool())
    val romData = Input(UInt(32.W))
    val instr = Output(UInt(32.W))
    val cond = Output(Bool())
  })

  val negClock = (~clock.asUInt).asBool.asClock
  val IR = withClock(negClock)(Reg(UInt(32.W)))

  io.instr := IR
  when (io.writeEN) {
    IR := io.romData
  }

  val condCode = io.romData(31, 28)

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