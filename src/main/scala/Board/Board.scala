package Board

import chisel3._
import chisel3.util._

import Controller._
import ALU._
import RegisterFile._
import FetchUnit._
import CPU._
import SevenSegmentDisplay._


class Board(instrs: Seq[String] = Seq(), realARM: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val segments = Output(UInt(8.W))
    val segmentsCtrl = Output(UInt(3.W))
    val segmentsEn = Output(Bool())
    val LEDs = Output(UInt(32.W))
    val switchs = Input(UInt(32.W))
    val buttons = Input(UInt(4.W))
    val clock_digit = Input(Clock())
  })

  val display = withClock(io.clock_digit)(Module(new SevenSegmentDisplay(digit = 8, div = 1<<17)))

  io.segments := display.io.seg
  io.segmentsCtrl := display.io.ctrl
  io.segmentsEn := display.io.en

  val cpu = Module(new CPU_Regs(instrs, realARM))

  //A B C F PC

  //数码管 LED
  //PC    A
  //PC    B
  //C     IR
  //F     writePC writeIR writeReg nzcv done err

  val digits = Wire(UInt(32.W))

  display.io.digits(0) := digits(31,28)
  display.io.digits(1) := digits(27,24)
  display.io.digits(2) := digits(23,20)
  display.io.digits(3) := digits(19,16)
  display.io.digits(4) := digits(15,12)
  display.io.digits(5) := digits(11,8)
  display.io.digits(6) := digits(7,4)
  display.io.digits(7) := digits(3,0)

  digits := MuxLookup(io.buttons,0.U, Array(
    "b1000".U -> cpu.io.PC,
    "b0100".U -> cpu.io.PC,
    "b0010".U -> cpu.io.C,
    "b0001".U -> cpu.io.F,
  ))

  io.LEDs := MuxLookup(io.buttons,0.U, Array(
    "b1000".U -> cpu.io.A,
    "b0100".U -> cpu.io.B,
    "b0010".U -> cpu.io.IR,
    "b0001".U -> Cat(cpu.io.writePC,"b010".U,cpu.io.writeIR,"b010".U,cpu.io.writeReg,"b010".U,cpu.io.nzcv,"b010".U,cpu.io.done,"b010".U,cpu.io.err),
  ))
}

object Board_Gen extends App {
  val instrs = Seq(
    "he3a01064", // MOV     r1, #100
    "he3a00000", // MOV     r0, #0
    "he0800001", // ADD     r0, r0, r1 @ loop
    "he2511001", // SUBS    r1, r1, #1
    "h1afffffc", // BNE     loop
  )
  chisel3.emitVerilog(new Board(instrs, realARM = true), Array("--target-dir", "gen"))
}
