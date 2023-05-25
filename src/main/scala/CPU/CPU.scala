package CPU

import chisel3._

import Controller._
import ALU._
import RegisterFile._
import FetchUnit._

class CPU_Regs(instrs: Seq[String] = Seq(), realARM: Boolean = false) extends Module {
  // 按照 ARM 手册上的说法，在执行指令时 PC 的值应为当前指令地址 + 8 而非 + 4
  // 这与实验指导书上的描述不一致（应为实验指导书为了简化实现而做出的修改）
  // 所以加入 realARM 参数来控制是否使用 ARM 手册上的 PC 计算方式
  // 具体操作为在寄存器堆读取 R15 时值时，若 realARM 为真则将其值加 4
  val io = IO (new Bundle{
    val writePC = Output(Bool())
    val writeIR = Output(Bool())
    val writeReg = Output(Bool())
    val A = Output(UInt(32.W))
    val B = Output(UInt(32.W))
    val C = Output(UInt(32.W))
    val F = Output(UInt(32.W))
    val IR = Output(UInt(32.W))
    val PC = Output(UInt(8.W))
    val nzcv = Output(UInt(4.W))
    val done = Output(Bool())
    val err = Output(Bool())
    val regs = Output(Vec(34, UInt(32.W)))
  })

  val negClock = (~clock.asUInt).asBool.asClock

  val nextState = withClock(negClock)(Reg(UInt(4.W)))
  val state = RegNext(nextState, 0.U)

  val controller = Module(new Controller(realARM))
  val alu = Module(new ALU)
  val registerFile = Module(new RegisterFile(realARM))
  val fetchUnit = Module(new FetchUnit(instrs))

  val IR = withClock(negClock)(Reg(UInt(32.W)))
  val PC = RegNext(registerFile.io.rPC, 0.U)
  // 加入一个 PC 寄存器作为缓冲是为了我们能同时写入 PC 和 IR
  val CPSR = withClock(negClock)(RegInit(VecInit(Seq.fill(32)(0.B))))
  val regs = withClock(negClock)(RegInit(VecInit(Seq.fill(4)(0.U(32.W)))))
  // 用于在微程序中记录数据的寄存器

  controller.io.regsR := regs
  for (i <- 0 until 4) {
    when (controller.io.regsWE(i)) {
      regs(i) := controller.io.regsW(i)
    }
  }

  registerFile.io.wPC := fetchUnit.io.pcNext
  fetchUnit.io.pc := PC

  registerFile.io.wData := alu.io.out
  registerFile.io.mode := "b10000".U

  fetchUnit.io.nzcv := CPSR.asUInt(31, 28)

  controller.io.state := state
  controller.io.IR := IR
  controller.io.cond := fetchUnit.io.cond
  when (controller.io.writeIR) {
    IR := fetchUnit.io.instr
  }

  controller.io.rDataA := registerFile.io.rDataA
  controller.io.rDataB := registerFile.io.rDataB
  controller.io.rDataC := registerFile.io.rDataC
  registerFile.io.rAddrA := controller.io.rAddrA
  registerFile.io.rAddrB := controller.io.rAddrB
  registerFile.io.rAddrC := controller.io.rAddrC
  registerFile.io.wAddr := controller.io.wAddr
  registerFile.io.wReg := controller.io.writeR
  registerFile.io.wPCReg := controller.io.writePC

  alu.io.a := controller.io.aluA
  alu.io.b := controller.io.aluB
  alu.io.c := controller.io.aluC
  alu.io.cin := CPSR(29)
  alu.io.op := controller.io.aluOp
  alu.io.mul_op := controller.io.aluMulOp
  alu.io.shift_op := controller.io.aluShiftOp
  alu.io.shift_num := controller.io.aluShiftNum
  controller.io.aluOut := alu.io.out
  controller.io.aluCout := alu.io.cout

  when (controller.io.writeN) {
    CPSR(31) := alu.io.nout
  }
  when (controller.io.writeZ) {
    CPSR(30) := alu.io.zout
  }
  when (controller.io.writeC) {
    CPSR(29) := alu.io.cout
  }
  when (controller.io.writeV) {
    CPSR(28) := alu.io.vout
  }

  val inc = Module(new Inc(4))
  inc.io.in := state
  nextState := Mux(controller.io.done, 0.U, inc.io.out)

  // Outputs
  io.writePC := controller.io.writePC
  io.writeIR := controller.io.writeIR
  io.writeReg := controller.io.writeR
  io.A := controller.io.aluA
  io.B := controller.io.aluB
  io.C := 0.U
  io.F := alu.io.out
  io.IR := IR
  io.PC := registerFile.io.rPC
  io.nzcv := CPSR.asUInt(31, 28)
  io.done := (((fetchUnit.io.instr === 0.U) && (state === 0.U)) || controller.io.err)
  io.err := controller.io.err
  io.regs := registerFile.io.regs
}

class CPU_Top(instrs: Seq[String] = Seq(), realARM: Boolean = false) extends Module {
  // Almost same as CPU_Regs, but without registers exposed
  val io = IO(new Bundle {
    val writePC = Output(Bool())
    val writeIR = Output(Bool())
    val writeReg = Output(Bool())
    val A = Output(UInt(32.W))
    val B = Output(UInt(32.W))
    val C = Output(UInt(32.W))
    val F = Output(UInt(32.W))
    val IR = Output(UInt(32.W))
    val PC = Output(UInt(8.W))
    val nzcv = Output(UInt(4.W))
    val done = Output(Bool())
    val err = Output(Bool())
  })

  val cpu = Module(new CPU_Regs(instrs, realARM))

  io.writePC := cpu.io.writePC
  io.writeIR := cpu.io.writeIR
  io.writeReg := cpu.io.writeReg
  io.A := cpu.io.A
  io.B := cpu.io.B
  io.C := cpu.io.C
  io.F := cpu.io.F
  io.IR := cpu.io.IR
  io.PC := cpu.io.PC
  io.nzcv := cpu.io.nzcv
  io.done := cpu.io.done
  io.err := cpu.io.err
}

object CPU_Gen extends App {
  val instrs = Seq(
    "he3a00012",
    "he1a00400",
    "he28000d6",
    "he1a00400",
    "he2800087",
    "he3a01001",
    "he1a01401",
    "he28110e2",
    "he1a01401",
    "he2811040",
    "he3a02004",
    "he1a02402",
    "he28220d2",
    "he12021c0",
  )
  chisel3.emitVerilog(new CPU_Top(instrs, realARM = true), Array("--target-dir", "gen"))
}