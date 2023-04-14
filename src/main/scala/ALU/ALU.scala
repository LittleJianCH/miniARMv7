package ALU

import chisel3._
import chisel3.util._

import BarrelShifter._

object ALUSim {
  def calc(_a: Int, _b: Int, cin: Int, op: Int): Long = {
    val a: Long = _a.toLong & ((1L << 32) - 1);
    val b: Long = _b.toLong & ((1L << 32) - 1);

    op match {
      case 0 => a & b
      case 1 => a ^ b
      case 2 => a - b
      case 3 => b - a
      case 4 => a + b
      case 5 => a + b + cin
      case 6 => a - b + cin - 1
      case 7 => b - a + cin - 1
      case 8 => a
      case 10 => a - b + 4
      case 12 => a | b
      case 13 => b
      case 14 => a & (~b)
      case 15 => ~b
    }
  }
}

class ALU_Core extends Module {
  // In terms of implementation, we have divided ALU into two layers.
  // The inner layer is the basic arithmetic module,
  // and the outer layer is a combination of arithmetic modules and barrel shifters.
  // The inner layer should look like a wrapper of Adder.

  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val op = Input(UInt(4.W))
    val cin = Input(UInt(1.W))
    val out = Output(UInt(32.W))
    val cout = Output(UInt(1.W))
  })

  val adder = Module(new Adder(32))
  val incA = Module(new Inc(32))
  val incB = Module(new Inc(32))
  val inc4 = Module(new Inc(30))
  val negatorA = Module(new Negator(32))
  val negatorB = Module(new Negator(32))

  incA.io.in := io.a
  negatorA.io.in := Mux(io.op === "b0111".U, incA.io.out, io.a)

  incB.io.in := io.b
  negatorB.io.in := Mux(io.op === "b0110".U, incB.io.out, io.b)

  adder.io.a := MuxCase(io.a, Array(
    (io.op === "b0011".U || io.op === "b0111".U) -> negatorA.io.out,
    (io.op === "b1101".U) -> 0.U
  ))

  adder.io.b := MuxCase(io.b, Array(
    (io.op === "b0010".U || io.op === "b0110".U || io.op === "b1010".U) -> negatorB.io.out,
    (io.op === "b1000".U) -> 0.U
  ))

  adder.io.cin := Mux(io.op === "b101".U || io.op === "b110".U || io.op === "b111".U, io.cin, 0.U)

  inc4.io.in := adder.io.sum(31, 2)

  io.out := Mux(io.op === "b1010".U, Cat(inc4.io.out, adder.io.sum(1, 0)), adder.io.sum)
  io.cout := (adder.io.cout ^ Mux(io.op === "b0110".U, incB.io.carry, 0.U)
                            ^ Mux(io.op === "b0111".U, incA.io.carry, 0.U)
                            ^ Mux(io.op === "b1010".U, inc4.io.carry, 0.U)
                            ^ Mux(io.op === "b0010".U || io.op === "b0011".U
                               || io.op === "b0110".U || io.op === "b0111".U
                               || io.op === "b1010".U, 1.U, 0.U))
}

class ALU extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val op = Input(UInt(4.W))
    val cin = Input(UInt(1.W))
    val shift_op = Input(UInt(3.W))
    val shift_num = Input(UInt(8.W))
    val out = Output(UInt(32.W))
    val nout = Output(UInt(1.W))
    val zout = Output(UInt(1.W))
    val cout = Output(UInt(1.W))
    val vout = Output(UInt(1.W))
  })

  val alu_core = Module(new ALU_Core)
  val barrel_shifter = Module(new BarrelShifter)

  barrel_shifter.io.Shift_OP := io.shift_op
  barrel_shifter.io.Shift_Num := io.shift_num
  barrel_shifter.io.Shift_Data := io.b
  barrel_shifter.io.Carry_Flag := io.cin

  alu_core.io.op := io.op
  alu_core.io.a := io.a
  alu_core.io.b := barrel_shifter.io.Shift_Out
  alu_core.io.cin := io.cin

  val realB = barrel_shifter.io.Shift_Out
  io.out := MuxLookup(io.op, alu_core.io.out, Array(
    "b0000".U -> (io.a & realB),
    "b0001".U -> (io.a ^ realB),
    "b1100".U -> (io.a | realB),
    "b1110".U -> (io.a & (~realB).asUInt),
    "b1111".U -> ~realB,
  ))

  io.nout := io.out(31)
  io.zout := !io.out.orR
  io.cout := MuxLookup(io.op, alu_core.io.cout, Array(
    "b0000".U -> barrel_shifter.io.Shift_Carry_Out,
    "b0001".U -> barrel_shifter.io.Shift_Carry_Out,
    "b1100".U -> barrel_shifter.io.Shift_Carry_Out,
    "b1110".U -> barrel_shifter.io.Shift_Carry_Out,
    "b1111".U -> barrel_shifter.io.Shift_Carry_Out,
  ))
  io.vout := alu_core.io.a(31) ^ alu_core.io.b(31) ^ io.out(31) ^ io.cout
}

object ALU_Gen extends App {
  chisel3.emitVerilog(new ALU, Array("--target-dir", "gen"))
}
