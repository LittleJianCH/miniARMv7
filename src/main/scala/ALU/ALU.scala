package ALU

import chisel3._
import chisel3.util._

import BarrelShifter._

// 简：我不知道这样做对不对，但是我想让生成的电路尽可能的接近我的想法
class And(width: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val out = Output(UInt(width.W))
  })

  io.out := io.a & io.b
}

class Or(width: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val out = Output(UInt(width.W))
  })

  io.out := io.a | io.b
}

class Xor(width: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val out = Output(UInt(width.W))
  })

  io.out := io.a ^ io.b
}

class Inv(width: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val out = Output(UInt(width.W))
  })

  io.out := ~io.in
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
    (io.op === "b0010".U || io.op === "b0110".U) -> negatorB.io.out,
    (io.op === "b1000".U) -> 0.U
  ))

  adder.io.cin := Mux(io.op >= "b101".U && io.op <= "b111".U, io.cin, 0.U)

  inc4.io.in := adder.io.sum(31, 2)

  io.out := Mux(io.op === "b1010".U, Cat(inc4.io.out, adder.io.sum(1, 0)), adder.io.sum)
  io.cout := (adder.io.cout ^ Mux(io.op === "b1010".U, incB.io.carry, 0.U)
                            ^ Mux(io.op === "b1011".U, incA.io.carry, 0.U)
                            ^ Mux(io.op === "b1010".U, inc4.io.carry, 0.U))
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
  val and = Module(new And(32))
  val or = Module(new Or(32))
  val xor = Module(new Xor(32))
  val inv = Module(new Inv(32))

  barrel_shifter.io.Shift_OP := io.shift_op
  barrel_shifter.io.Shift_Num := io.shift_num
  barrel_shifter.io.Shift_Data := io.b
  barrel_shifter.io.Carry_Flag := io.cin

  alu_core.io.op := io.op
  alu_core.io.a := io.a
  alu_core.io.b := barrel_shifter.io.Shift_Out
  alu_core.io.cin := io.cin

  inv.io.in := io.b
  and.io.a := Mux(io.op === "b1110".U, ~0.U, io.a)
  and.io.b := Mux(io.op === "b1110".U || io.op === "b1111".U, inv.io.out, io.b)

  or.io.a := io.a
  or.io.b := io.b

  xor.io.a := io.a
  xor.io.b := io.b

  io.out := MuxLookup(io.op, alu_core.io.out, Array(
    "b0000".U -> and.io.out,
    "b0001".U -> xor.io.out,
    "b1100".U -> or.io.out,
    "b1110".U -> and.io.out,
    "b1111".U -> and.io.out,
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
