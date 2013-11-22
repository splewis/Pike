package Pike

import scala.collection.mutable.MutableList

class Pike {

  /* Register information  */
  private val NumRegisters: Int = 16
  private val registers = new Array[Register](NumRegisters)

  /* Runtime variables */
  private var shouldKill = false
  private var instructionNumber = 0
  private var instructions = new MutableList[Instruction]()

  /* Register data structures */
  abstract sealed class Register
  case class IntRegister(value: Int) extends Register
  case class DoubleRegister(value: Double) extends Register
  case class StringRegister(value: String) extends Register

  /* Register helper functions */
  private def getRegister(rName: String): Register = {
    return registers(getRegisterIndex(rName))
  }

  private def getRegisterIndex(rName: String): Int = {
    if (rName.charAt(0) != 'r')
      throw new RuntimeException("Illegal Register name: " + rName)
    val index: Int = Integer.parseInt(rName.substring(1))
    if (index < 0 || index >= NumRegisters)
      throw new RuntimeException("Illegal Register number: " + index)
    return index
  }

  /* Helper functions for the runtime system */
  private def nextInstruction(): Unit = {
    goto(instructionNumber + 1)
  }

  private def goto(n: Int): Unit = {
    instructionNumber = n
    if (shouldKill || instructionNumber >= instructions.size || instructionNumber < 0)
      println("Program terminated")
    else
      instructions(instructionNumber).action()
  }

  /* run command: begins program execution - this is NOT an instruction! */
  def run(): Unit = instructions(0).action()

  /*
   * Instruction data structures. 
   *  - Each Instruction should extend Instruction and override the action method.
   *  - the end of action should specify how to continue after running the instruction
   */
  abstract class Instruction {
    instructions += this
    def action(): Unit
    def apply() = action()
  }

  /* mov instruction: moves a int/double/string into a register */
  case class mov(value: Any, rName: String) extends Instruction {
    override def action() = {
      var newReg: Register = null
      value match {
        case d: Int => newReg = new IntRegister(d)
        case d: Float => newReg = new DoubleRegister(d)
        case d: Double => newReg = new DoubleRegister(d)
        case d: String => newReg = new StringRegister(d)
        case _ => throw new RuntimeException("Illegal type put in " + rName);
      }
      registers(getRegisterIndex(rName)) = newReg
      nextInstruction()
    }
  }

  /* jmp instruction: jumps to the nth instruction and starts running at it */
  case class jmp(n: Int) extends Instruction {
    override def action() = goto(n)
  }

  /* jz instruction: jumps to the nth instruction if the int-valued register is 0 */
  case class jz(n: Int, r: String) extends Instruction {
    override def action() = {
      if (getIntValue(r) == 0) goto(n)
      else nextInstruction()
    }
  }
  /* reljz instruction: jumps ahead n instruction if the int-valued register is 0 */
  case class reljz(n: Int, r: String) extends Instruction {
    override def action() = {
      if (getIntValue(r) == 0) goto(instructionNumber + n)
      else nextInstruction()
    }
  }

  /* jpos instruction: jumps to the nth instruction if the int-valued register is positive */
  case class jpos(n: Int, r: String) extends Instruction {
    override def action() = {
      if (getIntValue(r) > 0) goto(n)
      else nextInstruction()
    }
  }
  /* reljpos instruction: jumps to the nth instruction if the int-valued register is positive */
  case class reljpos(n: Int, r: String) extends Instruction {
    override def action() = {
      if (getIntValue(r) > 0) goto(instructionNumber + n)
      else nextInstruction()
    }
  }

  /* jneg instruction: jumps to the nth instruction if the int-valued register is negative */
  case class jneg(n: Int, r: String) extends Instruction {
    override def action() = {
      if (getIntValue(r) < 0) goto(n)
      else nextInstruction()
    }
  }
  /* reljneg instruction: jumps to the nth instruction if the int-valued register is negative */
  case class reljneg(n: Int, r: String) extends Instruction {
    override def action() = {
      if (getIntValue(r) < 0) goto(instructionNumber + n)
      else nextInstruction()
    }
  }

  /* kill instruction: ends program execution */
  object kill extends Instruction {
    override def action() = {} // i.e. do nothing
  }

  /* Helper function for int operations */
  protected def getIntValue(r: String): Int = {
    val reg: Register = getRegister(r)
    if (reg.isInstanceOf[IntRegister])
      return reg.asInstanceOf[IntRegister].value
    else
      throw new RuntimeException(r + " does not contain an integer.")
  }

  /* Helper function for double operations */
  protected def getDoubleValue(r: String): Double = {
    val reg: Register = getRegister(r)
    if (reg.isInstanceOf[DoubleRegister])
      return reg.asInstanceOf[DoubleRegister].value
    else
      throw new RuntimeException(r + " does not contain a floating point value.")
  }

  /* iprint instruction: prints integer in register */
  case class iprint(r: String) extends Instruction {
    def action() = {
      println(getIntValue(r))
      nextInstruction()
    }
  }

  /*fprint instruction: prints floating point value in register */
  case class fprint(r: String) extends Instruction {
    def action() = {
      println(getDoubleValue(r))
      nextInstruction()
    }
  }

  /* add instruction: adds integers from 2 registers and puts the result in r3 */
  case class add(r1: String, r2: String, r3: String) extends Instruction {
    override def action() = mov(getIntValue(r1) + getIntValue(r2), r3).action()
  }

  /* mul instruction: multiplies integers from 2 registers and puts the result in r3 */
  case class mul(r1: String, r2: String, r3: String) extends Instruction {
    override def action() = mov(getIntValue(r1) * getIntValue(r2), r3).action()
  }

  /* idiv instruction: divides integers from 2 registers and puts the result in r3 */
  case class idiv(r1: String, r2: String, r3: String) extends Instruction {
    override def action() = mov(getIntValue(r1) / getIntValue(r2), r3).action()
  }

  /* mod instruction: takes the modulus integers from 2 registers and puts the result in r3 */
  case class mod(r1: String, r2: String, r3: String) extends Instruction {
    override def action() = mov(getIntValue(r1) % getIntValue(r2), r3).action()
  }

  /* inc instruction: increments the integer value from a register by 1 */
  case class inc(r1: String) extends Instruction {
    override def action() = mov(getIntValue(r1) + 1, r1).action()
  }

  /* dec instruction: decrements the integer value from a register by 1 */
  case class dec(r1: String) extends Instruction {
    override def action() = mov(getIntValue(r1) - 1, r1).action()
  }

  /* fadd instruction: adds floating point numbers from 2 registers and puts the result in r3 */
  case class fadd(r1: String, r2: String, r3: String) extends Instruction {
    override def action() = mov(getDoubleValue(r1) + getDoubleValue(r2), r3).action()
  }

  /* fmul instruction: adds floating point numbers from 2 registers and puts the result in r3 */
  case class fmul(r1: String, r2: String, r3: String) extends Instruction {
    override def action() = mov(getDoubleValue(r1) * getDoubleValue(r2), r3).action()
  }

  /* fdiv instruction: adds floating point numbers from 2 registers and puts the result in r3 */
  case class fdiv(r1: String, r2: String, r3: String) extends Instruction {
    override def action() = mov(getDoubleValue(r1) / getDoubleValue(r2), r3).action()
  }

  /* Debug/testing/display functions. Not intended to be part of the language. */
  protected def registerInfo(rName: String): String = {
    val reg: Register = getRegister(rName)
    val titleStr = "Register " + rName + " info:"
    val infoStr = {
      reg match {
        case IntRegister(data) => "Type=Int, Data=" + data
        case StringRegister(data) => "Type=String, Data=" + data
        case DoubleRegister(data) => "Type=Double, Data=" + data
        case _ => "Unhandled Register type"
      }
    }
    return titleStr + "\n" + infoStr
  }

  protected def printRegisterInfo(rName: String): Unit = {
    println(registerInfo(rName))
  }

}
