package Pike

import org.junit.Assert._
import org.junit.Test
import org.junit.Before

class TestSuite {

  val epsilon: Double = 1e-8

  @Test
  def movTests() = movProgram.runner
  object movProgram extends Pike {
    def runner() = {
      mov(1, r0)
      mov(1.0, r1)
      mov(-1.00000006, r2)
      mov(10000, r5)
      mov(-12345, r9)
      run
      assertEquals(1, getIntValue(r0))
      assertEquals(1.0, getDoubleValue(r1), epsilon)
      assertEquals(-1.00000006, getDoubleValue(r2), epsilon)
      assertEquals(10000, getIntValue(r5))
      assertEquals(-12345, getIntValue(r9))
    }
  }

  @Test
  def accumTest() = accumProgram.runner
  object accumProgram extends Pike {
    def runner() = {
      /* computes 10 + 9 + ... + 1 = 55 and puts it in r0 */
      mov(0, r0) // summation register
      mov(10, r1) // temp register
      add(r0, r1, r0) // relative jump lands here
      dec(r1)
      reljpos(-2, r1) // relative jump 2 instructions back
      run
      assertEquals(55, getIntValue(r0))
    }
  }

  @Test
  def sumSq() = sumSqProgram.runner
  object sumSqProgram extends Pike {
    def runner() = {
      /* computes 10^2 + 9^2 + ... + 1^2 == 385  and puts it in r0 */
      mov(0, r0) // summation register
      mov(10, r1) // temp register for i=[1..10]
      mov(0, r2) // temp register for i=[1..10]
      label("loop")
      mov(r1, r2)
      mul(r2, r2, r2) // square r2
      add(r0, r2, r0) // r0 += r2 
      dec(r1)
      jpos("loop", r1)
      run
      assertEquals(385, getIntValue(r0))
    }
  }

}
