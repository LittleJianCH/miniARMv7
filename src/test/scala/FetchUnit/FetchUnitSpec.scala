package FetchUnit

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class FetchUnitSpec extends AnyFreeSpec with ChiselScalatestTester {
  "fetch unit examples" in {
    test(new FetchUnit(Seq(
      "b0000" + "00" * 12 + "0101",
      "b0001" + "10" * 12 + "1100",
      "b0010" + "11" * 12 + "0001",
      "b0011" + "01" * 12 + "1000",
      "b0100" + "10" * 12 + "1010",
      "b0101" + "00" * 12 + "1111",
      "b0110" + "11" * 12 + "0000",
      "b0111" + "11" * 12 + "1011",
      "b1000" + "10" * 12 + "1101",
      "b1001" + "01" * 12 + "0111",
      "b1010" + "00" * 12 + "0010",
      "b1011" + "10" * 12 + "1011",
      "b1100" + "01" * 12 + "1011",
      "b1101" + "11" * 12 + "0101",
      "b1110" + "10" * 12 + "1110",
      "b1111" + "00" * 12 + "0011",
    ))) { p =>
      //"0000" + "00" * 12 + "0101"

      p.io.pc.poke(0.U)
      
      p.io.nzcv.poke("b0000".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0001".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0010".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0011".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0100".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0101".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0110".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0111".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1000".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1001".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1010".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1011".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1100".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1101".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1110".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1111".U)
      p.io.pcNext.expect(4.U)
      p.io.instr.expect(("b0000" + "00" * 12 + "0101").U)
      p.io.cond.expect(true.B)

      //"0001" + "10" * 12 + "1100"

      p.io.pc.poke(4.U)
      
      p.io.nzcv.poke("b0000".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0001".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0010".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0011".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0100".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0101".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0110".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0111".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1000".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1001".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1010".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1011".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1100".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1101".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1110".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1111".U)
      p.io.pcNext.expect(8.U)
      p.io.instr.expect(("b0001" + "10" * 12 + "1100").U)
      p.io.cond.expect(false.B)

      //"0010" + "11" * 12 + "0001",

      p.io.pc.poke(8.U)

      p.io.nzcv.poke("b0000".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0001".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0010".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0011".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0100".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0101".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0110".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0111".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1000".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1001".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1010".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1011".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1100".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1101".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1110".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1111".U)
      p.io.pcNext.expect(12.U)
      p.io.instr.expect(("b0010" + "11" * 12 + "0001").U)
      p.io.cond.expect(true.B)

      //"0011" + "01" * 12 + "1000",

      p.io.pc.poke(12.U)

      p.io.nzcv.poke("b0000".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0001".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0010".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0011".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0100".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0101".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0110".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0111".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1000".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1001".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1010".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1011".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1100".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1101".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1110".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1111".U)
      p.io.pcNext.expect(16.U)
      p.io.instr.expect(("b0011" + "01" * 12 + "1000").U)
      p.io.cond.expect(false.B)

      //"0100" + "10" * 12 + "1010",

      p.io.pc.poke(16.U)

      p.io.nzcv.poke("b0000".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0001".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0010".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0011".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0100".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0101".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0110".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0111".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1000".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1001".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1010".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1011".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1100".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1101".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1110".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1111".U)
      p.io.pcNext.expect(20.U)
      p.io.instr.expect(("b0100" + "10" * 12 + "1010").U)
      p.io.cond.expect(true.B)

      //"0101" + "00" * 12 + "1111",

      p.io.pc.poke(20.U)

      p.io.nzcv.poke("b0000".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0001".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0010".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0011".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0100".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0101".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0110".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0111".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1000".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1001".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1010".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1011".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1100".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1101".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(false.B)
      
      p.io.nzcv.poke("b1110".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1111".U)
      p.io.pcNext.expect(24.U)
      p.io.instr.expect(("b0101" + "00" * 12 + "1111").U)
      p.io.cond.expect(false.B)

      //"0110" + "11" * 12 + "0000",

      p.io.pc.poke(24.U)

      p.io.nzcv.poke("b0000".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0001".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0010".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0011".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0100".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0101".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b0110".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b0111".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1000".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1001".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1010".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1011".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1100".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1101".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(true.B)

      p.io.nzcv.poke("b1110".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(false.B)

      p.io.nzcv.poke("b1111".U)
      p.io.pcNext.expect(28.U)
      p.io.instr.expect(("b0110" + "11" * 12 + "0000").U)
      p.io.cond.expect(true.B)

      for(pc <- 7 to 16){
        p.io.pc.poke(pc*4)

        for(nzcv <- 0 to 15){
          p.io.nzcv.poke(("b" + nzcv.toBinaryString).U)
          p.io.pcNext.expect((pc+1)*4)

          if(pc == 7){
            p.io.instr.expect(("b0111" + "11" * 12 + "1011").U)
            if(nzcv%2 == 0){
              p.io.cond.expect(true.B)
            }
            else{
              p.io.cond.expect(false.B)
            }
          }
          else if(pc == 8){
            p.io.instr.expect(("b1000" + "10" * 12 + "1101").U)
            if((nzcv&0x2)!=0 && (nzcv&0x4)==0){
              p.io.cond.expect(true.B)
            } 
            else{
              p.io.cond.expect(false.B)
            } 
          }
          else if(pc == 9){
            p.io.instr.expect(("b1001" + "01" * 12 + "0111").U)
            if((nzcv&0x2)==0 || (nzcv&0x4)!=0){
              p.io.cond.expect(true.B)
            } 
            else{
              p.io.cond.expect(false.B)
            }
          }
          else if(pc == 10){
            p.io.instr.expect(("b1010" + "00" * 12 + "0010").U)
            if((nzcv&0x8)>>3 == (nzcv&0x1)){
              p.io.cond.expect(true.B)
            }
            else{
              p.io.cond.expect(false.B)
            }
          }
          else if(pc == 11){
            p.io.instr.expect(("b1011" + "10" * 12 + "1011").U)
            if((nzcv&0x8)>>3 != (nzcv&0x1)){
              p.io.cond.expect(true.B)
            }
            else{
              p.io.cond.expect(false.B)
            }
          }
          else if(pc == 12){
            p.io.instr.expect(("b1100" + "01" * 12 + "1011").U)
            if((nzcv&0x4)==0 && (nzcv&0x8)>>3 == (nzcv&0x1)){
              p.io.cond.expect(true.B)
            }
            else{
              p.io.cond.expect(false.B)
            }
          }
          else if(pc == 13){
            p.io.instr.expect(("b1101" + "11" * 12 + "0101").U)
            if((nzcv&0x4)!=0 || (nzcv&0x8)>>3 != (nzcv&0x1)){
              p.io.cond.expect(true.B)
            }
            else{
              p.io.cond.expect(false.B)
            }
          }
          else if(pc == 14){
            p.io.instr.expect(("b1110" + "10" * 12 + "1110").U)
            p.io.cond.expect(true.B)
          }
          else if(pc == 15){
            p.io.instr.expect(("b1111" + "00" * 12 + "0011").U)
            p.io.cond.expect(true.B)
          }
        }
      }
    }
  }
}
