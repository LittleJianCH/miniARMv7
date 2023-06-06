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

  "test cpu Halfword multiply" in {
    test(new CPU_Regs(Seq(
      "he3a00008", // mov r0, #18
      "he1a00500", // mov r0, r0, LSL #10
      "he28000d6", // add r0, #214
      "he1a00400", // mov r0, r0, LSL #8
      "he2800087", // add r0, #135
      "he3a01063", // mov r1, #99
      "he1a01481", // mov r1, r1, LSL #9
      "he28110e2", // add r1, #226
      "he1a01401", // mov r1, r1, LSL #8
      "he2811040", // add r1, #64
      "he3a02004", // mov r2, #4
      "he1a02402", // mov r2, r2, LSL #8
      "he28220d2", // add r2, #210
      "he12021c0", // SMLAWT r0, r0, r1, r2
      "he3a03017", // mov r3, #23
      "he3a04019", // mov r4, #25
      "he1630483", // SMULBB r3, r3, r4
      "he1410382", // SMLALBB r0, r1, r2, r3
    ), realARM = true)) { p =>
      while (!p.io.done.peekBoolean()) {
        p.clock.step()
      }

      val regs = p.io.regs
      regs(0).expect("haf1e5".U)
      regs(1).expect("hc6e240".U)
      regs(2).expect("h4d2".U)
      regs(3).expect("h23f".U)
      regs(4).expect("h19".U)
    }
  }

  "test cpu LDR/STR" in {
    test(new CPU_Regs(Seq(
      "he24dd010", // int main() {
      "he3a00000", //   int a = 1234567;
      "he58d000c", //   int b = 7654321;
      "he59f1020", //   int c = a + b;
      "he58d1008", //   return 0;
      "he59f101c", // }
      "he58d1004",
      "he59d1008",
      "he59d2004",
      "he0811002",
      "he58d1000",
      "he28dd010",
      "h00000000",
      "h0012d687",
      "h0074cbb1",
    ), realARM = true)) { p =>
      while (!p.io.done.peekBoolean()) {
        p.clock.step()
      }

      val regs = p.io.regs
      regs(1).expect("h0087a238".U)
      regs(2).expect("h0074cbb1".U)
    }
  }

  "test cpu load/store multiple" in {
    test(new CPU_Regs(Seq(
      /*
      mov r0, #1
      mov r1, #2
      mov r3, #45
      mov r5, #123
      push {r0, r1, r3, r5}
      mov r0, #0
      mov r1, #0
      mov r3, #0
      mov r5, #0
      pop {r0, r1, r3, r5}
       */
      "he3a00001",
      "he3a01002",
      "he3a0302d",
      "he3a0507b",
      "he92d002b",
      "he3a00000",
      "he3a01000",
      "he3a03000",
      "he3a05000",
      "he8bd002b",
    ), realARM = true)) { p =>
      while (!p.io.done.peekBoolean()) {
        p.clock.step()
      }

      val regs = p.io.regs
      regs(0).expect(1.U)
      regs(1).expect(2.U)
      regs(3).expect(45.U)
      regs(5).expect(123.U)
      regs(13).expect(0.U)
    }
  }

  "test cpu Fibonacci" in {
    test(new CPU_Regs(Seq(
      /*
      int fib(int n) {
          return n <= 1 ? 1 : fib(n - 1) + fib(n - 1);
      }

      int main() {
          int x = 5;
          int y = fib(x);
          return 0;
      }
      */
      "he92d4800",
      "he1a0b00d",
      "he24dd010",
      "he3a00000",
      "he58d0000",
      "he50b0004",
      "he3a00005",
      "he58d0008",
      "he59d0008",
      "heb000005",
      "he1a01000",
      "he59d0000",
      "he58d1004",
      "he1a0d00b",
      "he8bd4800",
      "h00000000",
      "he92d4800",
      "he1a0b00d",
      "he24dd010",
      "he50b0004",
      "he51b0004",
      "he3500001",
      "hca000003",
      "heaffffff",
      "he3a00001",
      "he58d0008",
      "hea00000b",
      "he51b0004",
      "he2400001",
      "hebfffff1",
      "he58d0004",
      "he51b0004",
      "he2400002",
      "hebffffed",
      "he1a01000",
      "he59d0004",
      "he0800001",
      "he58d0008",
      "heaffffff",
      "he59d0008",
      "he1a0d00b",
      "he8bd4800",
      "he12fff1e",
    ), realARM = true)) { p =>
      while (!p.io.done.peekBoolean()) {
        p.clock.step()
      }

      val regs = p.io.regs
      regs(1).expect(8.U)
    }
  }
}
