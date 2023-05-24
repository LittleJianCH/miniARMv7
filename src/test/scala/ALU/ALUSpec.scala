package ALU

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import BarrelShifter._

class ALUSpec extends AnyFreeSpec with ChiselScalatestTester {
  def toUnsigned(i: Long): BigInt = {
    BigInt(java.lang.Long.toUnsignedString(i))
  }

  def toUInt(i: Long): UInt = {
    toUnsigned(i).asUInt
  }

  val TEST_TIMES = 100

  "test alu" in {
    test(new ALU) { p =>
      for (i <- 0 to TEST_TIMES) {
        val supportOps = Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 12, 13, 14, 15)

        val aluOp = supportOps(scala.util.Random.nextInt(supportOps.length))
        val aluA = scala.util.Random.nextInt()
        val aluB = scala.util.Random.nextInt()
        val aluCin = scala.util.Random.nextInt(2)
        val barrelShifterOp = scala.util.Random.nextInt(8)
        val barrelShifterNum = scala.util.Random.nextInt(32)

        p.io.a.poke(toUInt(aluA)(31, 0))
        p.io.b.poke(toUInt(aluB)(31, 0))
        p.io.cin.poke(aluCin.U)
        p.io.op.poke(aluOp.U)
        p.io.shift_op.poke(barrelShifterOp.U)
        p.io.shift_num.poke(barrelShifterNum.U)

        val barrelShifterResult = BarrelShifterSim.calc(aluB, barrelShifterOp, barrelShifterNum, aluCin)
        val aluResult = ALUSim.calc(aluA, barrelShifterResult._1, aluCin, aluOp)

        if (Array(0, 1, 12, 14, 15).contains(aluOp)) {
          p.io.cout.expect(barrelShifterResult._2.U)
        } else if (Array(2, 3, 4, 5, 6, 7, 10).contains(aluOp)) {
          p.io.cout.expect(aluResult._2.B)
        }

        p.io.out.expect(toUInt(aluResult._1)(31, 0))
        p.io.zout.expect((aluResult._1 == 0).B)
      }
    }
  }

  "test alu example" in {
    test(new ALU) { p =>
      p.io.a.poke(32.U)
      p.io.b.poke(32.U)
      p.io.op.poke("b0100".U)
      p.io.cin.poke(0.U)
      p.io.shift_op.poke("b000".U)
      p.io.shift_num.poke(0.U)

      p.io.out.expect(64.U)
    }
  }
}
