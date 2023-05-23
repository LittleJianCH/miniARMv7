package CPU

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

import Controller._
import ALU._
import RegisterFile._
import FetchUnit._

class CPU_Regs(instrs: Seq[String] = Seq()) extends Module {
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
    val regs = Output(Vec(34, UInt(32.W)))
  })

  val negClock = (~clock.asUInt).asBool.asClock

  val nextState = withClock(negClock)(Reg(UInt(4.W)))
  val state = RegNext(nextState, 0.U)

  val controller = Module(new Controller)
  val alu = Module(new ALU)
  val registerFile = Module(new RegisterFile)
  val fetchUnit = Module(new FetchUnit(instrs))

  val IR = withClock(negClock)(Reg(UInt(32.W)))
  val CPSR = withClock(negClock)(RegInit(VecInit(Seq.fill(32)(0.B))))

  registerFile.io.wPC := fetchUnit.io.pcNext
  fetchUnit.io.pc := registerFile.io.rPC

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
  alu.io.cin := CPSR(29)
  alu.io.op := controller.io.aluOp
  alu.io.shift_op := controller.io.aluShiftOp
  alu.io.shift_num := controller.io.aluShiftNum

  when (controller.io.writeNZCV) {
    CPSR(31) := alu.io.nout
    CPSR(30) := alu.io.zout
    CPSR(29) := alu.io.cout
    CPSR(28) := alu.io.vout
  }

  nextState := Mux(controller.io.done, 0.U, state + 1.U)

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
  io.done := (fetchUnit.io.instr === 0.U)
  io.regs := registerFile.io.regs
}

class CPU_Top(instrs: Seq[String] = Seq()) extends Module {
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
  })

  val cpu = Module(new CPU_Regs(instrs))

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
}

object CPU_Gen extends App {
  val instrs = Seq(
    "11110011101100000000000000000011", //r0<-3
    "11110011101100000001000100000100", //r1<-(4 >> 1)
    "11110010010100000000000000000011", //SUBS r0 #3
  )
  chisel3.emitVerilog(new CPU_Top(instrs), Array("--target-dir", "gen"))
}