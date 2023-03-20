package BarrelShifter

import chisel3._
import chisel3.util._

class BarrelShifter extends Module{
  val io = IO(new Bundle{
    val Shift_OP = Input(UInt(3.W))
    val Shift_Data = Input(UInt(32.W))
    val Shift_Num = Input(UInt(8.W))
    val Carry_Flag = Input(UInt(1.W))

    val Shift_Out = Output(UInt(32.W))
    val Shift_Carry_Out = Output(UInt(1.W))
  })

  // get first two bits of SHIFT_OP
  private val ShiftType = io.Shift_OP(2, 1)

  when (io.Shift_OP(0) === 0.U && io.Shift_Num === 0.U) {
    // no shift
    io.Shift_Out := io.Shift_Data
    io.Shift_Carry_Out := 0.U
  }.elsewhen (ShiftType === "b00".U) {
    // lsl
    when (io.Shift_Num === 0.U) {
      io.Shift_Out := io.Shift_Data
      io.Shift_Carry_Out := 0.U
    }.elsewhen (io.Shift_Num < 32.U) {
      io.Shift_Out := io.Shift_Data << io.Shift_Num
      io.Shift_Carry_Out := io.Shift_Data(32.U - io.Shift_Num)
    }.otherwise {
      io.Shift_Out := 0.U
      io.Shift_Carry_Out := 0.U
    }
  }.elsewhen (ShiftType === "b01".U) {
    // lsr
    when (io.Shift_Num === 0.U) {
      io.Shift_Out := 0.U
      io.Shift_Carry_Out := io.Shift_Data(31)
    }.elsewhen (io.Shift_Num < 32.U) {
      io.Shift_Out := io.Shift_Data >> io.Shift_Num
      io.Shift_Carry_Out := io.Shift_Data(io.Shift_Num - 1.U)
    }.otherwise {
      io.Shift_Out := 0.U
      io.Shift_Carry_Out := 0.U
    }
  }.elsewhen (ShiftType === "b10".U) {
    // asr
    when (io.Shift_Num === 0.U) {
      io.Shift_Out := Fill(32, io.Shift_Data(31))
      io.Shift_Carry_Out := io.Shift_Data(31)
    }.elsewhen (io.Shift_Num < 32.U) {
      io.Shift_Out := Cat(Fill(32, io.Shift_Data(31)), io.Shift_Data) >> io.Shift_Num
      io.Shift_Carry_Out := io.Shift_Data(io.Shift_Num - 1.U)
    }.otherwise {
      io.Shift_Out := Fill(32, io.Shift_Data(31))
      io.Shift_Carry_Out := io.Shift_Data(31)
    }
  }.otherwise {
    // rxr/ror
    when (io.Shift_Num === 0.U) {
      io.Shift_Out := Cat(io.Carry_Flag, io.Shift_Data(31, 1))
      io.Shift_Carry_Out := io.Shift_Data(0)
    }.otherwise {
      val ShiftNum = io.Shift_Num % 32.U
      io.Shift_Out := Cat(io.Shift_Data, io.Shift_Data) >> ShiftNum
      io.Shift_Carry_Out := io.Shift_Data(ShiftNum - 1.U)
    }
  }
}

object Main extends App {
  chisel3.emitVerilog(new BarrelShifter, Array("--target-dir", "gen"))
}