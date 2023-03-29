package ALU

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class AdderSpec extends AnyFreeSpec with ChiselScalatestTester {
  val TEST_TIMES = 500
  val TEST_WIDTH = 16
  // the test runs *very* slow when TEST_WIDTH is large (like 32)
  // maybe there is some optimization to do?

  "test adder" in {
    test(new Adder(TEST_WIDTH)) { p =>
      for (i <- 0 until TEST_TIMES) {
        val a = scala.util.Random.nextLong(1L << TEST_WIDTH)
        val b = scala.util.Random.nextLong(1L << TEST_WIDTH)
        val s0 = a + b
        val s1 = a + b + 1
        p.io.a.poke(a.U)
        p.io.b.poke(b.U)
        p.io.cin.poke(0.U)
        p.io.sum.expect(s0.U(TEST_WIDTH - 1, 0))
        p.io.cout.expect(s0.U(TEST_WIDTH))
        p.io.cin.poke(1.U)
        p.io.sum.expect(s1.U(TEST_WIDTH - 1, 0))
        p.io.cout.expect(s1.U(TEST_WIDTH))
      }
    }
  }
}
