package BarrelShifter

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class BarrelShifterSpec extends AnyFreeSpec with ChiselScalatestTester {
  "test logic shift left " in {
    test(new BarrelShifter) { c =>
      c.io.Shift_OP.poke("b001".U)
      c.io.Shift_Data.poke("h12345678".U)
      c.io.Shift_Num.poke(0.U)
      c.io.Carry_Flag.poke(0.U)
      c.io.Shift_Out.expect("h12345678".U)
      c.io.Shift_Carry_Out.expect(0.U)

      c.io.Shift_Num.poke(4.U)
      c.io.Shift_Out.expect("h23456780".U)
      c.io.Shift_Carry_Out.expect(1.U)

      c.io.Shift_Num.poke(100.U)
      c.io.Shift_Out.expect("h00000000".U)
      c.io.Shift_Carry_Out.expect(0.U)

      c.io.Shift_Num.poke(0.U)
      c.io.Shift_OP.poke("b000".U)
      c.io.Shift_Out.expect("h00000000".U)
      c.io.Shift_Carry_Out.expect(0.U)
    }
  }

  "test logic shift right " in {
    test(new BarrelShifter) { c =>
      c.io.Shift_OP.poke("b011".U)
      c.io.Shift_Data.poke("h12345678".U)
      c.io.Shift_Num.poke(0.U)
      c.io.Carry_Flag.poke(0.U)
      c.io.Shift_Out.expect("h12345678".U)
      c.io.Shift_Carry_Out.expect(0.U)

      c.io.Shift_Num.poke(4.U)
      c.io.Shift_Out.expect("h01234567".U)
      c.io.Shift_Carry_Out.expect(1.U)

      c.io.Shift_Num.poke(100.U)
      c.io.Shift_Out.expect("h00000000".U)

      c.io.Shift_OP.poke("b010".U)
      c.io.Shift_Num.poke(0.U)
      c.io.Shift_Out.expect("h00000000".U)
    }
  }

  "test arithmetic shift right " in {
    test(new BarrelShifter) { c =>
      c.io.Shift_OP.poke("b101".U)
      c.io.Shift_Data.poke("h12345678".U)
      c.io.Shift_Num.poke(0.U)
      c.io.Carry_Flag.poke(0.U)
      c.io.Shift_Out.expect("h12345678".U)
      c.io.Shift_Carry_Out.expect(0.U)

      c.io.Shift_Num.poke(8.U)
      c.io.Shift_Out.expect("h00123456".U)

      c.io.Shift_Data.poke("h80000000".U)
      c.io.Shift_Num.poke(4.U)
      c.io.Shift_Out.expect("hf8000000".U)
      c.io.Shift_Carry_Out.expect(0.U)

      c.io.Shift_Num.poke(100.U)
      c.io.Shift_Out.expect("hffffffff".U)
      c.io.Shift_Carry_Out.expect(1.U)
    }
  }

  "test rotate right" in {
    test(new BarrelShifter) { c =>
      c.io.Shift_OP.poke("b111".U)
      c.io.Shift_Data.poke("h12345678".U)
      c.io.Shift_Num.poke(0.U)
      c.io.Carry_Flag.poke(0.U)
      c.io.Shift_Out.expect("h12345678".U)
      c.io.Shift_Carry_Out.expect(0.U)

      c.io.Shift_Num.poke(4.U)
      c.io.Shift_Out.expect("h81234567".U)
      c.io.Shift_Carry_Out.expect(1.U)

      c.io.Shift_Num.poke(100.U) // 100 % 32 = 4
      c.io.Shift_Out.expect("h81234567".U)
      c.io.Shift_Carry_Out.expect(1.U)

      c.io.Shift_Num.poke(0.U)
      c.io.Shift_Data.poke("h00000001".U)
      c.io.Shift_OP.poke("b110".U)
      c.io.Carry_Flag.poke(1.U)
      c.io.Shift_Out.expect("h80000000".U)
      c.io.Shift_Carry_Out.expect(1.U)
    }
  }
}
