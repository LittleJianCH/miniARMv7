package ALU

import chisel3._
import chisel3.util._

import BarrelShifter._

object ALUSim {
  def calc(_a: Int, _b: Int, cin: Int, op: Int): (Int, Boolean) = {
    val a: Long = _a.toLong
    val b: Long = _b.toLong

    val result = op match {
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

    (result.toInt, (((result >> 32) & 1) ^ ((op >> 1) & 1)) == 1)
  }
}

class Negator(width: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val out = Output(UInt(width.W))
  })

  val inc = Module(new Inc(width))
  inc.io.in := ~io.in
  io.out := inc.io.out
}

class AddUnit extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val op = Input(UInt(4.W))
    val cin = Input(UInt(1.W))
    val out = Output(UInt(32.W))
    val cout = Output(UInt(1.W))
  })

  val adder = Module(new Adder(33))
  val inc4 = Module(new Inc(31))
  val incA = Module(new Inc(33))
  val incB = Module(new Inc(33))
  val negatorA = Module(new Negator(33))
  val negatorB = Module(new Negator(33))

  val a33 = Cat(io.a(31), io.a)
  val b33 = Cat(io.b(31), io.b)

  incA.io.in := a33
  incB.io.in := b33

  negatorA.io.in := Mux(io.op === "b0111".U, incA.io.out, a33)
  negatorB.io.in := Mux(io.op === "b0110".U, incB.io.out, b33)

  adder.io.a := MuxCase(a33, Array(
    (io.op === "b0011".U || io.op === "b0111".U) -> negatorA.io.out,
    (io.op === "b1101".U) -> 0.U
  ))

  adder.io.b := MuxCase(b33, Array(
    (io.op === "b0010".U || io.op === "b0110".U || io.op === "b1010".U) -> negatorB.io.out,
    (io.op === "b1000".U) -> 0.U
  ))

  adder.io.cin := Mux(io.op === "b0101".U || io.op === "b0110".U || io.op === "b0111".U, io.cin, 0.U)

  inc4.io.in := adder.io.sum(32, 2)

  val r33 = Mux(io.op === "b1010".U, Cat(inc4.io.out, adder.io.sum(1, 0)), adder.io.sum)

  io.out := r33(31, 0)
  io.cout := r33(32) ^ io.op(1)
}

class MulUnit extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(16.W))
    val b = Input(UInt(16.W))
    val signed = Input(Bool())
    val out = Output(UInt(32.W))
  })

  val multiplier = Module(new Multiplier(16))
  val negatorA = Module(new Negator(16))
  val negatorB = Module(new Negator(16))
  val negatorF = Module(new Negator(32))

  negatorA.io.in := io.a
  negatorB.io.in := io.b
  negatorF.io.in := multiplier.io.out

  multiplier.io.a := Mux(io.signed && io.a(15), negatorA.io.out, io.a)
  multiplier.io.b := Mux(io.signed && io.b(15), negatorB.io.out, io.b)

  io.out := Mux(io.signed && (io.a(15) ^ io.b(15)),
    negatorF.io.out,
    multiplier.io.out
  )
}

class ALU extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val c = Input(UInt(16.W))
    val mul_op = Input(UInt(2.W))
    // bit0: if use mul unit, bit1: if signed
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

  val add_unit = Module(new AddUnit)
  val mul_unit = Module(new MulUnit)
  val barrel_shifter = Module(new BarrelShifter)

  barrel_shifter.io.Shift_OP := io.shift_op
  barrel_shifter.io.Shift_Num := io.shift_num
  barrel_shifter.io.Shift_Data := io.b
  barrel_shifter.io.Carry_Flag := io.cin

  mul_unit.io.a := io.a(15, 0)
  mul_unit.io.b := io.c
  mul_unit.io.signed := io.op(1)

  add_unit.io.op := io.op
  add_unit.io.a := Mux(io.mul_op(0), mul_unit.io.out, io.a)
  add_unit.io.b := barrel_shifter.io.Shift_Out
  add_unit.io.cin := io.cin

  val realB = barrel_shifter.io.Shift_Out
  io.out := MuxLookup(io.op, add_unit.io.out, Array(
    "b0000".U -> (io.a & realB),
    "b0001".U -> (io.a ^ realB),
    "b1100".U -> (io.a | realB),
    "b1110".U -> (io.a & (~realB).asUInt),
    "b1111".U -> ~realB,
  ))

  io.nout := io.out(31)
  io.zout := !io.out.orR
  io.cout := MuxLookup(io.op, add_unit.io.cout, Array(
    "b0000".U -> barrel_shifter.io.Shift_Carry_Out,
    "b0001".U -> barrel_shifter.io.Shift_Carry_Out,
    "b1100".U -> barrel_shifter.io.Shift_Carry_Out,
    "b1110".U -> barrel_shifter.io.Shift_Carry_Out,
    "b1111".U -> barrel_shifter.io.Shift_Carry_Out,
  ))
  io.vout := add_unit.io.a(31) ^ add_unit.io.b(31) ^ io.out(31) ^ io.cout
}

object ALU_Gen extends App {
  chisel3.emitVerilog(new ALU, Array("--target-dir", "gen"))
}
