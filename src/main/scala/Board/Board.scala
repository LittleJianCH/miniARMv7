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

  val display = withClock(io.clock_digit)(Module(new SevenSegmentDisplay(digit = 8, div = 1<<15)))

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
    "b0001".U -> Cat(
      cpu.io.writePC,
      "b010".U(3.W),
      cpu.io.writeIR,
      "b010".U(3.W),
      cpu.io.writeReg,
      "b010".U(3.W),
      cpu.io.nzcv,
      "b010".U(3.W),
      cpu.io.done,
      "b010".U(3.W),
      cpu.io.err
    ),
  ))
}

object Board_Gen extends App {
  val instrs = Seq(
    "he92d4800",
    "he1a0b00d",
    "he24dd010",
    "he3a00000",
    "he58d0000",
    "he50b0004",
    "he3a00005",
    "he58d0008",
    "he59d0008",
    "heb000005",
    "he1a01000",
    "he59d0000",
    "he58d1004",
    "he1a0d00b",
    "he8bd4800",
    "h00000000",
    "he92d4800",
    "he1a0b00d",
    "he24dd010",
    "he50b0004",
    "he51b0004",
    "he3500001",
    "hca000003",
    "heaffffff",
    "he3a00001",
    "he58d0008",
    "hea00000b",
    "he51b0004",
    "he2400001",
    "hebfffff1",
    "he58d0004",
    "he51b0004",
    "he2400002",
    "hebffffed",
    "he1a01000",
    "he59d0004",
    "he0800001",
    "he58d0008",
    "heaffffff",
    "he59d0008",
    "he1a0d00b",
    "he8bd4800",
    "he12fff1e",
  )
  chisel3.emitVerilog(new Board(instrs, realARM = true), Array("--target-dir", "gen"))
}
