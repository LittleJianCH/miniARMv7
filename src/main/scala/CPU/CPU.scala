package CPU

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

import Decoder._
import ALU._
import RegisterFile._
import FetchUnit._

class CPU_Regs(instrs: Seq[String] = Seq()) extends Module {
  object State extends ChiselEnum {
    val IF, ID, EX, WB = Value
    // FetchUnit, Controller, ALU, RegisterFile & NZCV
  }

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
    val regs = Output(Vec(34, UInt(6.W)))
  })

  val negClock = (~clock.asUInt).asBool.asClock

  val decoder = Module(new Decoder)
  val alu = Module(new ALU)
  val registerFile = Module(new RegisterFile)
  val fetchUnit = Module(new FetchUnit(instrs))

  val nzcv = withClock(negClock)(RegInit(0.U(4.W)))
  val IR = withClock(negClock)(RegInit(0.U(32.W)))
  val nextState = withClock(negClock)(RegInit(State.IF))
  val state = RegNext(nextState, State.IF)
  val cond = withClock(negClock)(RegInit(false.B))
  val doneReg = withClock(negClock)(RegInit(false.B))

  alu.io.shift_num := decoder.io.aluShiftNum
  alu.io.a := decoder.io.aluA
  alu.io.b := decoder.io.aluB
  alu.io.op := decoder.io.aluOp
  alu.io.shift_num := decoder.io.aluShiftNum
  alu.io.shift_op := decoder.io.aluShiftOp
  decoder.io.aluOut := alu.io.out

  registerFile.io.rAddrA := decoder.io.rAddrA
  registerFile.io.rAddrB := decoder.io.rAddrB
  registerFile.io.rAddrC := decoder.io.rAddrC
  decoder.io.rDataA := registerFile.io.rDataA
  decoder.io.rDataB := registerFile.io.rDataB
  decoder.io.rDataC := registerFile.io.rDataC
  registerFile.io.wData := decoder.io.wData
  registerFile.io.wAddr := decoder.io.wAddr
  registerFile.io.wReg := (cond && state === State.WB)

  registerFile.io.wPC := fetchUnit.io.pcNext
  registerFile.io.wPCReg := (state === State.IF)
  fetchUnit.io.pc := registerFile.io.rPC

  fetchUnit.io.nzcv := nzcv
  alu.io.cin := nzcv(1)

  registerFile.io.mode := "b10000".U // our cpu runs only in user mode now

  decoder.io.IR := IR

  when (!doneReg) {
    switch(state) {
      is(State.IF) {
        cond := fetchUnit.io.cond
        when(fetchUnit.io.cond) {
          IR := fetchUnit.io.instr
        }
        nextState := State.ID
      }

      is(State.ID) {
        doneReg := (IR === 0.U || doneReg)
        nextState := State.EX
      }

      is(State.EX) {
        nextState := State.WB
      }

      is(State.WB) {
        when(cond && decoder.io.nzcvEN) {
          nzcv := Cat(alu.io.nout, alu.io.zout, alu.io.cout, alu.io.vout)
        }

        nextState := State.IF
      }
    }
  }

  // Outputs
  io.writePC := (state === State.IF)
  io.writeIR := (state === State.IF && cond)
  io.writeReg := (state === State.WB && cond)
  io.A := decoder.io.aluA
  io.B := decoder.io.aluB
  io.C := 0.U
  io.F := decoder.io.aluOut
  io.IR := IR
  io.PC := registerFile.io.rPC
  io.nzcv := nzcv
  io.done := doneReg
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