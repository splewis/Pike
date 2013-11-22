package Pike

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList

class Pike {

  /* Runtime variables */
  private var shouldKill = false
  private var instructionsRead = false
  private var instructionNumber = 0
  private var instructions = new MutableList[Instruction]()
  private var labels = new HashMap[String, Int]()

  /* Register information  */
  private val NumRegisters: Int = 10 // Number of general purpose registers
  private val registers = new Array[Register](NumRegisters)

  /* Register data structures */
  abstract sealed class Register {
    val name = "undefined name"
  }
  case class IntRegister(value: Int) extends Register
  case class DoubleRegister(value: Double) extends Register

  /* Register instances - these lets you write r0 instead of "r0" */
  // This is somewhat of a bad hack.
  object r0 extends Register { override val name = "r0" }
  object r1 extends Register { override val name = "r1" }
  object r2 extends Register { override val name = "r2" }
  object r3 extends Register { override val name = "r3" }
  object r4 extends Register { override val name = "r4" }
  object r5 extends Register { override val name = "r5" }
  object r6 extends Register { override val name = "r6" }
  object r7 extends Register { override val name = "r7" }
  object r8 extends Register { override val name = "r8" }
  object r9 extends Register { override val name = "r9" }
  implicit def reg2str(r: Register): String = r.name

  /* Register helper functions */
  private def getRegister(rName: String): Register = {
    return registers(getRegisterIndex(rName))
  }

  private def getRegisterIndex(rName: String): Int = {
    if (rName.charAt(0) != 'r')
      throw new RuntimeException("Illegal Register name: " + rName)
    val index: Int = Integer.parseInt(rName.substring(1))
    if (index < 0 || index >= NumRegisters)
      readErr("Illegal register name: " + rName)
    return index
  }

  /* Helper functions for the runtime system */
  private def nextInstruction(): Unit = {
    goto(instructionNumber + 1)
  }

  private def goto(n: Int): Unit = {
    instructionNumber = n
    val outOfBounds = instructionNumber >= instructions.size || instructionNumber < 0
    if (!outOfBounds && !shouldKill)
      instructions(instructionNumber).action()
  }

  /* run command: begins program execution - this is NOT an instruction! */
  def run(): Unit = {
    instructionsRead = true // flag to prevent more instructions from being added to list
    goto(0)
  }

  /*
   * Instruction data structures. 
   *  - Each Instruction should extend Instruction and override the action method.
   *  - the end of action should specify how to continue after running the instruction
   */
  abstract class Instruction {
    if (!instructionsRead)
      instructions += this
    def action(): Unit
  }

  /* mov instruction: moves a int/double/string into a register */
  case class mov(value: Any, rName: String) extends Instruction {
    if (!legalType(value))
      readErr("Illegal value")
    override def action() = {
      val newReg: Register = makeRegister(value)
      registers(getRegisterIndex(rName)) = newReg
      nextInstruction()
    }
  }

  /* Internal register/value helpers */
  private def legalType(value: Any): Boolean = {
    return (value.isInstanceOf[Int] ||
      value.isInstanceOf[Double] ||
      value.isInstanceOf[Float] ||
      value.isInstanceOf[Register])
  }

  private def makeRegister(value: Any): Register = {
    value match {
      case x: Int => new IntRegister(x)
      case x: Float => new DoubleRegister(x)
      case x: Double => new DoubleRegister(x)
      case x: String => makeRegister(getValue(x))
      case x: Register => makeRegister(getValue(x.name))
      case _ => throw new RuntimeException("Illegal register value: " + value.toString)
    }
  }

  private def getValue(r: String): Any = {
    getRegister(r) match {
      case x: IntRegister => x.value
      case x: DoubleRegister => x.value
      case _ => throw new RuntimeException("Illegal register type: " + r)
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

  /* label instruction: names a point in the code */
  case class label(name: String) extends Instruction {
    labels(name) = instructions.size // current line number in reading
    override def action() = nextInstruction()
  }

  /* implicit conversion that allows jumps to labels*/
  implicit def label2Line(labelName: String) = labels(labelName)

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
      runErr(r + " does not contain an integer.")
  }

  /* Helper function for double operations */
  protected def getDoubleValue(r: String): Double = {
    val reg: Register = getRegister(r)
    if (reg.isInstanceOf[DoubleRegister])
      return reg.asInstanceOf[DoubleRegister].value
    else
      runErr(r + " does not contain a floating point value.")
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

  /* sub instruction: subtracts integers from 2 registers and puts the result in r3 */
  case class sub(r1: String, r2: String, r3: String) extends Instruction {
    override def action() = mov(getIntValue(r1) - getIntValue(r2), r3).action()
  }

  /* mul instruction: multiplies integers from 2 registers and puts the result in r3 */
  case class mul(r1: String, r2: String, r3: String) extends Instruction {
    override def action() = mov(getIntValue(r1) * getIntValue(r2), r3).action()
  }

  /* idiv instruction: divides integers from 2 registers and puts the result in r3 */
  case class div(r1: String, r2: String, r3: String) extends Instruction {
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

  /* fsub instruction: subtracts floating point numbers from 2 registers and puts the result in r3 */
  case class fsub(r1: String, r2: String, r3: String) extends Instruction {
    override def action() = mov(getDoubleValue(r1) - getDoubleValue(r2), r3).action()
  }

  /* fmul instruction: adds floating point numbers from 2 registers and puts the result in r3 */
  case class fmul(r1: String, r2: String, r3: String) extends Instruction {
    override def action() = mov(getDoubleValue(r1) * getDoubleValue(r2), r3).action()
  }

  /* fdiv instruction: adds floating point numbers from 2 registers and puts the result in r3 */
  case class fdiv(r1: String, r2: String, r3: String) extends Instruction {
    override def action() = mov(getDoubleValue(r1) / getDoubleValue(r2), r3).action()
  }

  /* Internal exceptions methods for readtime/runtime errors */
  private def readErr(str: String) = {
    val errStr = ("Syntax error at instruction " + instructions.size
      + " = " + instructions.last + ", " + str)
    throw new RuntimeException("\n" + errStr)
  }
  private def runErr(str: String) = {
    val errStr = ("Runtime error at instruction " + instructionNumber
      + " = " + instructions(instructionNumber) + ", " + str)
    throw new RuntimeException(errStr)
  }

  /* Debug/testing/display functions. Not intended to be part of the language. */
  protected def registerInfo(rName: String): String = {
    val reg: Register = getRegister(rName)
    val titleStr = "Register " + rName + " info:"
    val infoStr = {
      reg match {
        case IntRegister(data) => "Type=Int, Data=" + data
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
