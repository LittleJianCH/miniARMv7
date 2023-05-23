package CPU

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class CPUSpec extends AnyFreeSpec with ChiselScalatestTester {
  "test cpu example" in {
    test(new CPU_Regs(Seq(
      "b11110011101100000000000000000011", //r0<-3
      "b11110011101100000001000100000100", //r1<-(4 >> 1)
      "b11110010010100000000000000000011", //SUBS r0 #3
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
      "b11110011101100000000000000000011", //r0<-3
      "b11110011101100000001000100000100", //r1<-(4 >> 1)
      "b11110010010100000000000000000011", //SUBS r0 #3
      "b11111111111111111111111111111111", //error
    ))) { p =>
      while (!p.io.done.peekBoolean()) {
        p.clock.step(1)
      }

      p.io.err.expect(true.B)
    }
  }

  "test cpu BX" in {
    test(new CPU_Regs(Seq(
      "b11100011101100000000000000001000", //r0<-8
      "b11100011101100000001000100001100", //r1<-(12 >> 1)
      "b11100010010100010001000000000001", //SUBS r1 #1 -> r1
      "b11100011001100010000000000000011", //TEQ r1 #3
      "b00010001001011111111111100010000", //if !Z, BX r0
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
      "b11100011101100000000000000001000", //r0<-8
      "b11100011101100000001000100001100", //r1<-(12 >> 1)
      "b11100010010100010001000000000001", //SUBS r1 #1 -> r1
      "b11100011001100010000000000000011", //TEQ r1 #3
      "b00011010111111111111111111111101", //if !Z, B #-3
    ))) { p =>
      while (!p.io.done.peekBoolean()) {
        p.clock.step(1)
      }

      val regs = p.io.regs
      regs(0).expect(8.U)
      regs(1).expect(3.U)
    }
  }

  "test cpu: Sum of Numbers from 1 to 10" in {
    test(new CPU_Regs(Seq(
      "he3a0100a", // MOV     r1, #10
      "he3a00000", // MOV     r0, #0
      "he0800001", // ADD     r0, r0, r1 @ loop
      "he2511001", // SUBS    r1, r1, #1
      "h1afffffd", // BNE     loop
    ))) { p =>
      while (!p.io.done.peekBoolean()) {
        p.clock.step(1)
      }

      val regs = p.io.regs
      regs(0).expect(55.U)
      regs(1).expect(0.U)
    }
  }
}
