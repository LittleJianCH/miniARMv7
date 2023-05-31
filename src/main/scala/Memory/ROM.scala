package Memory

import chisel3._

class ROM(instrs: Seq[String] = Seq()) extends Module {
  val io = IO(new Bundle {
    val addrA = Input(UInt(32.W))
    val addrB = Input(UInt(32.W))
    val dataA = Output(UInt(32.W))
    val dataB = Output(UInt(32.W))
  })

  val rom = VecInit((0 to 63).map(i =>
    if (i < instrs.length) {
      instrs(i).U(32.W)
    } else {
      0.U(32.W)
    }
  ))

  io.dataA := rom(io.addrA(31, 2))
  io.dataB := rom(io.addrB(31, 2))
}
