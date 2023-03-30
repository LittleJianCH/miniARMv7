package ALU

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class MultiplierSpec extends AnyFreeSpec with ChiselScalatestTester {
  val TEST_TIMES = 200
  val TEST_WIDTH = 16

  "test multiplier" in {
    test(new Multiplier(TEST_WIDTH)) { p =>
      for (i <- 0 until TEST_TIMES) {
        val a = scala.util.Random.nextLong(1L << TEST_WIDTH)
        val b = scala.util.Random.nextLong(1L << TEST_WIDTH)
        p.io.a.poke(a.U)
        p.io.b.poke(b.U)
        p.io.out.expect((a * b).U)
      }
    }
  }
}
