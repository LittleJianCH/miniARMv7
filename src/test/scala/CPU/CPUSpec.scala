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

  "test cpu BX" in {
    test(new CPU_Regs(Seq(
      "11100011101100000000000000001000", //r0<-8
      "11100011101100000001000100001100", //r1<-(12 >> 1)
      "11100010010100010001000000000001", //SUBS r1 #1 -> r1
      "11100011001100010000000000000011", //TEQ r1 #3
      "00010001001011111111111100010000", //if !Z, BX r0
    ))) { p =>
      while (!p.io.done.peekBoolean()) {
        p.clock.step(1)
      }

      val regs = p.io.regs
      regs(0).expect(8.U)
      regs(1).expect(3.U)
    }
  }

  "test cpu B" in {
    test(new CPU_Regs(Seq(
      "11100011101100000000000000001000", //r0<-8
      "11100011101100000001000100001100", //r1<-(12 >> 1)
      "11100010010100010001000000000001", //SUBS r1 #1 -> r1
      "11100011001100010000000000000011", //TEQ r1 #3
      "00011010111111111111111111111101", //if !Z, B #-3
    ))) { p =>
      while (!p.io.done.peekBoolean()) {
        p.clock.step(1)
      }

      val regs = p.io.regs
      regs(0).expect(8.U)
      regs(1).expect(3.U)
    }
  }
}
