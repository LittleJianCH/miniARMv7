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

  "test cpu: Pow 2" in {
    test(new CPU_Regs(Seq(
      "he3a0100a", // MOV     r1, #10
      "heb000001", // BL      pow2
      "he1a02001", // MOV     r2, r1
      "hea000005", // B       end
      "he1a00001", // MOV     r0, r1 @ pow2
      "he3a01001", // MOV     r1, #1
      "he0811001", // ADD     r1, r1, r1 @ loop
      "he2500001", // SUBS    r0, r0, #1
      "h1afffffc", // BNE     loop
      "he12fff1e", // BX      lr
      // @ end
    ),
      realARM = true)) { p =>
      while (!p.io.done.peekBoolean()) {
        p.clock.step(1)
      }

      val regs = p.io.regs
      regs(0).expect(0.U)
      regs(1).expect(1024.U)
      regs(2).expect(1024.U)
    }
  }

  "test cpu SMLAWT" in {
    test(new CPU_Regs(Seq(
      "he3a00012", // mov r0, #18
      "he1a00400", // mov r0, r0, LSL #8
      "he28000d6", // add r0, #214
      "he1a00400", // mov r0, r0, LSL #8
      "he2800087", // add r0, #135
      "he3a01001", // mov r1, #1
      "he1a01401", // mov r1, r1, LSL #8
      "he28110e2", // add r1, #226
      "he1a01401", // mov r1, r1, LSL #8
      "he2811040", // add r1, #64
      "he3a02004", // mov r2, #4
      "he1a02402", // mov r2, r2, LSL #8
      "he28220d2", // add r2, #210
      "he12021c0", // SMLAWT r0, r0, r1, r2
    ), realARM = true)) { p =>
      while (!p.io.done.peekBoolean()) {
        p.clock.step()
      }

      val regs = p.io.regs
      regs(0).expect(1252.U)
    }
  }
}
