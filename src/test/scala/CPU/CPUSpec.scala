package CPU

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class CPUSpec extends AnyFreeSpec with ChiselScalatestTester {
  "test cpu example" in {
    test(new CPU_Regs(Seq(
      "11110011101100000000000000000011", //r0<-3
      "11110011101100000001000100000100", //r1<-(4 >> 1)
      "11110010010100000000000000000011", //SUBS r0 #3
    ))) { p =>
      while (!p.io.done.peekBoolean()) {
        p.clock.step(1)
      }

      val regs = p.io.regs
      regs(0).expect(0.U)
      regs(1).expect(2.U)
    }
  }

  "test cpu error" in {
    test(new CPU_Regs(Seq(
      "11110011101100000000000000000011", //r0<-3
      "11110011101100000001000100000100", //r1<-(4 >> 1)
      "11110010010100000000000000000011", //SUBS r0 #3
      "11111111111111111111111111111111", //error
    ))) { p =>
      while (!p.io.done.peekBoolean()) {
        p.clock.step(1)
      }

      p.io.err.expect(true.B)
    }
  }
}
