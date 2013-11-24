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
  def integerTest() = integerProgram.runner
  object integerProgram extends Pike {
    def runner(): Unit = {
      mov(2, r0) // r0 = 2
      mov(3, r1) // r1 = 3
      add(r0, r1, r2) // r2 = 2+3 = 5
      mul(r1, r2, r3) // r3 = 3*5 = 15
      div(r3, r1, r4) // r4 = 15/3 = 5
      mod(r3, r0, r5) // r5 = 15%2 = 1
      add(7, r5, r6) // r6 = 7+1 = 8
      add(r6, 21, r7) // r7 =8+21 = 29
      run
      assertEquals(2, getIntValue(r0))
      assertEquals(3, getIntValue(r1))
      assertEquals(5, getIntValue(r2))
      assertEquals(15, getIntValue(r3))
      assertEquals(5, getIntValue(r4))
      assertEquals(1, getIntValue(r5))
      assertEquals(8, getIntValue(r6))
      assertEquals(29, getIntValue(r7))
    }
  }

  @Test
  def doubleTest() = doubleProgram.runner
  object doubleProgram extends Pike {
    def runner() = {
      mov(2.5, r0) // r0 = 2.5
      mov(3.5, r1) // r1 = 3.5
      fadd(r0, r1, r2) // r2 = 2.5+3.5 = 6.0
      fmul(r1, r2, r3) // r3 = 3.5*6.0 = 21.0
      fdiv(r3, r1, r4) // r4 = 21.0/3.5 = 6.0
      run
      assertEquals(2.5, getDoubleValue(r0), epsilon)
      assertEquals(3.5, getDoubleValue(r1), epsilon)
      assertEquals(6.0, getDoubleValue(r2), epsilon)
      assertEquals(21.0, getDoubleValue(r3), epsilon)
      assertEquals(6.0, getDoubleValue(r4), epsilon)
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

  @Test
  def simpleFunction = simpleFunctionProgram.runner
  object simpleFunctionProgram extends Pike {
    def runner() = {
      func("add15")
      loadstack(1, r0)
      add(15, r0, r0)
      ret()

      mov(16, r2)
      push(r2)
      call("add15")
      pop(r2)
      run
      assertEquals(31, getIntValue(r0))
    }
  }

  @Test
  def sumSq_function = sumSqProgram_func.runner
  object sumSqProgram_func extends Pike {
    def runner() = {
      func("square")
      loadstack(1, r0)
      iprint(r0)
      mul(r0, r0, r0)
      ret()

      mov(0, r1) // summation register
      mov(10, r2) // temp register for i=[1..10]

      label("loop")
      push(r2)
      call("square")
      pop(r2)
      add(r0, r1, r1)
      dec(r2)
      jpos("loop", r2)

      mov(r1, r0)
      run
      assertEquals(385, getIntValue(r0))
    }
  }

  @Test
  def valueMadness = valueMadnessProgram.runner
  object valueMadnessProgram extends Pike {
    def runner() = {
      mov(15, r0)
      mov(r0, r1)
      mov(r0, r2)
      add(r1, r2, r3) // r3 has 30 in it now
      store(r2, 0) // mem(0) = r2 = 15
      store(r3, 1) // mem(1) = r3 = 30
      load(0, r5) // r5 = mem(0) = 15
      load(1, r6) // r6 = mem(1) = 30
      run
      assertEquals(15, getIntValue(r5))
      assertEquals(30, getIntValue(r6))
    }
  }

  @Test
  def stackLovingTest = stackLovingProgram.runner
  object stackLovingProgram extends Pike {
    def runner() = {
      mov(123, r0)
      mov(111, r1)
      push(r0)
      push(r1)
      pop(r2)
      pop(r3)
      mov(123.5, r4)
      push(r4)
      pop(r5)
      run
      assertEquals(0, getIntValue(rsp))
      assertEquals(111, getIntValue(r2))
      assertEquals(123, getIntValue(r3))
      assertEquals(123.5, getDoubleValue(r5), epsilon)
    }
  }

}
