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

  val tmp = io.Shift_Data(31)
  val fillBit = Mux(io.Shift_OP(2), io.Shift_Data(31), 0.U)
  val shiftType = io.Shift_OP(2, 1)
  val shiftData = Cat(Mux(io.Shift_OP(2, 1) === "b11".U, io.Shift_Data, Fill(32, fillBit)),
                      io.Shift_Data, Fill(32, 0.U))
  val shiftNum = Mux(io.Shift_Num === 0.U && io.Shift_OP(0) === 0.U,
                     32.U,
                     Mux(shiftType === "b11".U, io.Shift_Num % 32.U, io.Shift_Num))

  when (io.Shift_OP === "b110".U && io.Shift_Num === 0.U) {
    io.Shift_Out := Cat(io.Carry_Flag, io.Shift_Data(31, 1))
    io.Shift_Carry_Out := io.Shift_Data(0)
  }.elsewhen (shiftNum >= 32.U) {
    io.Shift_Out := Fill(32, fillBit)
    io.Shift_Carry_Out := fillBit
  }.otherwise {
    io.Shift_Out := VecInit((0 to 31).map(i =>
      shiftData(32.U + i.U + Mux(shiftType === "b00".U, -shiftNum, shiftNum)))
    ).asUInt
    io.Shift_Carry_Out := Mux(shiftType === "b00".U, shiftData(64.U - shiftNum), shiftData(shiftNum + 31.U))
  }
}

object Main extends App {
  chisel3.emitVerilog(new BarrelShifter, Array("--target-dir", "gen"))
}