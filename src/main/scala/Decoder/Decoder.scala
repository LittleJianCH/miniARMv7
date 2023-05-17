package Decoder

import chisel3._
import chisel3.util._

class Decoder extends Module {
  val io = IO(new Bundle {
    // For FetchUnit
    val IR = Input(UInt(32.W))

    // For Register File
    val rDataA = Input(UInt(32.W))
    val rDataB = Input(UInt(32.W))
    val rDataC = Input(UInt(32.W))
    val rAddrA = Output(UInt(4.W))
    val rAddrB = Output(UInt(4.W))
    val rAddrC = Output(UInt(4.W))
    val wAddr = Output(UInt(4.W))
    val wData = Output(UInt(32.W))

    // For ALU
    val aluA = Output(UInt(32.W))
    val aluB = Output(UInt(32.W))
    val aluOp = Output(UInt(4.W))
    val aluShiftOp = Output(UInt(3.W))
    val aluShiftNum = Output(UInt(8.W))
    val aluOut = Input(UInt(32.W))

    // For NZCV
    val nzcvEN = Output(Bool())
  })

  val I = io.IR
  val cond = I(31, 28)
  val OP = I(24, 21)
  val S = I(20)
  val rn = I(19, 16)
  val rd = I(15, 12)

  // 3.U will be treated as errorType temporarily
  val DPtype = MuxCase(3.U, Array(
    (I(27, 25) === "b000".U && I(4) === 0.U && rd =/= 15.U) -> 0.U,
    (I(27, 25) === "b000".U && I(7) === 0.U && I(4) === 1.U && rd =/= 15.U) -> 1.U,
    (I(27, 25) === "b001".U && rd =/= 15.U) -> 2.U,
  ))

  // some default values to pass the compilation
  io.rAddrB := 0.U
  io.rAddrC := 0.U
  io.aluB := 0.U
  io.aluShiftOp := 0.U
  io.aluShiftNum := 0.U

  when (DPtype === 0.U) {
    io.rAddrB := I(3, 0)
    io.aluB := io.rDataB
    io.aluShiftOp := Cat(I(6, 5), 0.U)
    io.aluShiftNum := I(11, 7)
  } .elsewhen (DPtype === 1.U) {
    io.rAddrB := I(3, 0)
    io.aluB := io.rDataB
    io.aluShiftOp := Cat(I(6, 5), 1.U)
    io.aluShiftNum := I(11, 8)
  } .elsewhen (DPtype === 2.U) {
    io.aluB := I(7, 0)
    io.aluShiftOp := "b111".U
    io.aluShiftNum := I(11, 8)
  }

  io.rAddrA := rn
  io.aluA := io.rDataA
  io.aluOp := OP

  io.wAddr := rd
  io.wData := io.aluOut

  io.nzcvEN := S
}
