package FetchUnit

import chisel3._

class InstrROM extends BlackBox {
  val io = IO(new Bundle {
    val clka = Input(Clock())
    val addra = Input(UInt(6.W))
    val douta = Output(UInt(32.W))
  })
}