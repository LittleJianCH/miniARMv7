package SevenSegmentDisplay

import chisel3._
import chisel3.util._

class SegmentDriver extends Module {
  val io = IO(new Bundle {
    val digit = Input(UInt(4.W))
    val seg = Output(UInt(8.W))
  })

  val segMap = VecInit(
    "b00000011".U, // 0
    "b10011111".U, // 1
    "b00100101".U, // 2
    "b00001101".U, // 3
    "b10011001".U, // 4
    "b01001001".U, // 5
    "b01000001".U, // 6
    "b00011111".U, // 7
    "b00000001".U, // 8
    "b00001001".U, // 9
    "b00010001".U, // A
    "b11000001".U, // b
    "b01100011".U, // C
    "b10000101".U, // d
    "b01100001".U, // E
    "b01110001".U  // F
  )

  io.seg := segMap(io.digit)
}

class SevenSegmentDisplay(digit: Int) extends Module {
  val io = IO(new Bundle {
    val digits = Input(Vec(digit, UInt(4.W)))
    val seg = Output(UInt(8.W))
    val ctrl = Output(UInt(log2Ceil(digit).W))
    val en = Output(Bool())
  })

  val counter = RegInit(0.U(log2Ceil(digit).W))
  val driver = Module(new SegmentDriver)

  counter := (counter + 1.U) % digit.U

  driver.io.digit := io.digits(counter)
  io.seg := driver.io.seg
  io.ctrl := counter
  io.en := true.B
}