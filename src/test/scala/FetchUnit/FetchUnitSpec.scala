package FetchUnit

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class FetchUnitSpec extends AnyFreeSpec with ChiselScalatestTester {
  "fetch unit examples" in {
    test(new FetchUnit(Seq(
      "0001" + "0" * 26 + "00",
      "0000" + "0" * 26 + "01",
      "1010" + "0" * 26 + "10",
      "1101" + "0" * 26 + "11",
    ))) { p =>
      p.io.nzcv.poke("b1110".U)
      p.io.pc.poke(0.U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0001" + "0" * 26 + "00").U)
      p.io.cond.expect(false.B)

      p.io.pc.poke(4.U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0000" + "0" * 26 + "01").U)
      p.io.cond.expect(true.B)

      p.io.pc.poke(8.U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b1010" + "0" * 26 + "10").U)
      p.io.cond.expect(false.B)

      p.io.pc.poke(12.U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b1101" + "0" * 26 + "11").U)
      p.io.cond.expect(true.B)
    }
  }
}
