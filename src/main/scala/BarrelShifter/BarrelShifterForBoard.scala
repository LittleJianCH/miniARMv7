package BarrelShifter

import chisel3._
import chisel3.util._

class BarrelShifterForBoard extends Module {
  // 由于板上按键不够，我们通过对 BarrelShifter 再次包装的方式在板上验收
  // 当 XX_Loader 为 1 时，将 Input_Data 的值写入 XX
  // 当 Output_Chooser 为 0 时，将 Shift_Out 的值输出，为 1 时，将 Shift_Carry_Out 的值输出
  val io = IO(new Bundle {
    val OP_Loader = Input(UInt(1.W))
    val Data_Loader = Input(UInt(1.W))
    val Num_Loader = Input(UInt(1.W))
    val Carry_Flag_Loader = Input(UInt(1.W))
    val Output_Chooser = Input(UInt(1.W))
    val Input_Data = Input(UInt(32.W))
    val Output_Data = Output(UInt(32.W))
  })

  val shifter = Module(new BarrelShifter)
  val op = RegInit(0.U(3.W))
  val data = RegInit(0.U(32.W))
  val num = RegInit(0.U(8.W))
  val carryFlag = RegInit(0.U(1.W))

  shifter.io.Shift_OP := op
  shifter.io.Shift_Data := data
  shifter.io.Shift_Num := num
  shifter.io.Carry_Flag := carryFlag
  io.Output_Data := Mux(io.Output_Chooser === 0.U,
                        shifter.io.Shift_Out,
                        Cat(Fill(31, 0.U), shifter.io.Shift_Carry_Out))

  when (io.OP_Loader === 1.U) {
    op := io.Input_Data(2, 0)
  }
  when (io.Data_Loader === 1.U) {
    data := io.Input_Data
  }
  when (io.Num_Loader === 1.U) {
    num := io.Input_Data(7, 0)
  }
  when(io.Carry_Flag_Loader === 1.U) {
    carryFlag := io.Input_Data(0)
  }
}

object BarrelShifterForBoardGen extends App {
  chisel3.emitVerilog(new BarrelShifterForBoard, Array("--target-dir", "gen"))
}
