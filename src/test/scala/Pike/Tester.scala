package Pike

import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class TestSuite {

  val epsilon: Double = 1e-8

  @Test
  def movTests() = {
    movProgram.runner
  }
  object movProgram extends Pike {
    def runner() = {
      mov(1, "r0")
      mov(1.0, "r1")
      mov(-1.00000006, "r2")
      mov(10000, "r5")
      mov(-12345, "r15")
      run
      assertEquals(1, getIntValue("r0"))
      assertEquals(1.0, getDoubleValue("r1"), epsilon)
      assertEquals(-1.00000006, getDoubleValue("r2"), epsilon)
      assertEquals(10000, getIntValue("r5"))
      assertEquals(-12345, getIntValue("r15"))
    }
  }

}

