package Pike

import scala.collection.mutable.HashMap
import scala.collection.mutable.MutableList

class Pike(val MemSize: Int = 1024) {

  /* Runtime variables */
  private var shouldKill = false
  private var instructionsRead = false
  private var instructionNumber = 0
  private var instructions = new MutableList[Instruction]()
  private var labels = new HashMap[String, Int]()
  private var unresolvedLabels = new HashMap[Int, String]()
  private var functions = new HashMap[String, Int]()

  /* Register information  */
  private val GeneralPurposeRegisters: Int = 10 // Number of registers allocated
  private val MemStartIndex = GeneralPurposeRegisters + 3 // +2 for rsp rbp, +1 for tmpRegister
  private val registers = new Array[Register](MemStartIndex + MemSize)

  /** Internal Register data structure */
  abstract class Register
  case class IntRegister(value: Int) extends Register
  case class DoubleRegister(value: Double) extends Register

  /** Internal storage for memory/register references. */
  abstract class Container(name: String, index: Int) {
    registers(index) = new IntRegister(0)
    def getRegister(): Register = registers(index)
    def setRegister(r: Register): Unit = (registers(index) = r)
    override def toString = name
  }

  case class RegisterContainer(name: String, index: Int) extends Container(name, index)
  case class MemoryContainer(name: String, index: Int) extends Container(name, index)

  /* Register instances */
  var r0 = new RegisterContainer("r0", 0)
  var r1 = new RegisterContainer("r1", 1)
  var r2 = new RegisterContainer("r2", 2)
  var r3 = new RegisterContainer("r3", 3)
  var r4 = new RegisterContainer("r4", 4)
  var r5 = new RegisterContainer("r5", 5)
  var r6 = new RegisterContainer("r6", 6)
  var r7 = new RegisterContainer("r7", 7)
  var r8 = new RegisterContainer("r8", 8)
  var r9 = new RegisterContainer("r9", 9)
  private var tmpRegister = new RegisterContainer("tmp", 10)
  var rsp = new RegisterContainer("rsp", 11)
  var rbp = new RegisterContainer("rbp", 12)

  private val memory = new Array[MemoryContainer](MemSize)

  /* Helper functions for the runtime system */
  private def nextInstruction(): Unit = {
    goto(instructionNumber + 1)
  }

  /* Internal runtime function - this is where the magic happens! */
  private def goto(n: Int): Unit = {
    instructionNumber = n
    if (instructionNumber < 0) {
      // then n was a unresolved label ID, try to look it up
      try {
        instructionNumber = unresolvedLabels(instructionNumber)
      } catch {
        case e: NoSuchElementException => runErr("missing label")
      }
    }

    val outOfBounds = instructionNumber >= instructions.size || instructionNumber < 0
    if (!outOfBounds && !shouldKill) {
      val i = instructions(instructionNumber)
      i.action()
      i.next()
    }
  }

  /** run command: begins program execution - this is NOT an instruction! */
  def run(): Unit = {
    instructionsRead = true // flag to prevent more instructions from being added to list
    goto(0)
  }

  /**
   * Internal Instruction data structures.
   *  - Each Instruction should extend Instruction
   *  - the action method specifies what it should do at runtime,
   *      it defaults to doing nothing
   *  - the next method specifies how to continue when the instruction is done,
   *      it defaults to going to the next instructions
   */
  abstract class Instruction {
    if (!instructionsRead)
      instructions += this
    def action(): Unit = {}
    def next(): Unit = nextInstruction()
  }

  /** conversion of Int->the memory cell index by the Int, used for store command */
  implicit def memoryLocation2Container(index: Int): MemoryContainer = {
    val result = memory(index)
    if (result == null)
      memory(index) = new MemoryContainer("mem@" + index, MemStartIndex + index)
    return memory(index)
  }

  /** store instruction: puts the value into a memory cell */
  case class store(value: Any, r: MemoryContainer) extends Instruction {
    if (!legalType(value))
      readErr("Illegal value: " + value)
    override def action() = {
      val newReg: Register = makeRegister(value)
      r.setRegister(newReg)
    }
  }

  /** load instruction: loads a value from a memory cell into a register */
  case class load(index: Int, r: RegisterContainer) extends Instruction {
    override def action() = {
      val newReg: Register = makeRegister(memory(index))
      r.setRegister(newReg)
    }
  }

  /** loadstack instruction: reads a function argument from the stack  (offset=1) is the first argument */
  case class loadstack(offset: Int, r: RegisterContainer) extends Instruction {
    override def action() = {
      load(getIntValue(rbp) + offset - 3, r).action()
    }
  }

  /** push instruction: pushes value in a register onto the stack */
  case class push(r: RegisterContainer) extends Instruction {
    override def action() = {
      inc(rsp).action()
      store(r, getIntValue(rsp)).action()
    }
  }

  /** pop instruction: pushes value in a register onto the stack */
  case class pop(r: RegisterContainer) extends Instruction {
    override def action() = {
      load(getIntValue(rsp), r).action()
      dec(rsp).action()
    }
  }

  /** mov instruction: moves a int/double/string into a register */
  case class mov(value: Any, r: RegisterContainer) extends Instruction {
    if (!legalType(value))
      readErr("Illegal value: " + value)
    override def action() = {
      val newReg: Register = makeRegister(value)
      r.setRegister(newReg)
    }
  }

  /** Internal register/value helpers */
  private def legalType(value: Any): Boolean = {
    assert(value != null);
    return (value.isInstanceOf[Int] ||
      value.isInstanceOf[Double] ||
      value.isInstanceOf[Float] ||
      value.isInstanceOf[Register] ||
      value.isInstanceOf[RegisterContainer])
  }

  private def makeRegister(value: Any): Register = {
    if (value == null)
      return new IntRegister(0)
    value match {
      case x: Int => new IntRegister(x)
      case x: Float => new DoubleRegister(x)
      case x: Double => new DoubleRegister(x)
      case x: Register => makeRegister(getValue(x))
      case x: Container => makeRegister(x.getRegister)
      case _ => throw new RuntimeException("Illegal register value: " + value.toString)
    }
  }

  implicit def container2Register(r: RegisterContainer): Register = r.getRegister

  private def getValue(r: Register): Any = {
    assert(r != null);
    r match {
      case x: IntRegister => x.value
      case x: DoubleRegister => x.value
      case _ => throw new RuntimeException("Illegal register type: " + r)
    }
  }

  /** jmp instruction: jumps to the nth instruction and starts running at it */
  case class jmp(n: Int) extends Instruction {
    override def next() = goto(n)
  }
  
  /** jz instruction: jumps to the nth instruction if the int-valued register is 0 */
  case class jz(n: Int, r: RegisterContainer) extends Instruction {
    override def next() = {
      if (getIntValue(r) == 0) goto(n)
      else nextInstruction()
    }
  }
  /** reljz instruction: jumps ahead n instruction if the int-valued register is 0 */
  case class reljz(n: Int, r: RegisterContainer) extends Instruction {
    override def next() = {
      if (getIntValue(r) == 0) goto(instructionNumber + n)
      else nextInstruction()
    }
  }

  /** jpos instruction: jumps to the nth instruction if the int-valued register is positive */
  case class jpos(n: Int, r: RegisterContainer) extends Instruction {
    override def next() = {
      if (getIntValue(r) > 0) goto(n)
      else nextInstruction()
    }
  }
  /** reljpos instruction: jumps to the nth instruction if the int-valued register is positive */
  case class reljpos(n: Int, r: RegisterContainer) extends Instruction {
    override def next() = {
      if (getIntValue(r) > 0) goto(instructionNumber + n)
      else nextInstruction()
    }
  }

  /** jneg instruction: jumps to the nth instruction if the int-valued register is negative */
  case class jneg(n: Int, r: RegisterContainer) extends Instruction {
    override def next() = {
      if (getIntValue(r) < 0) goto(n)
      else nextInstruction()
    }
  }
  /** reljneg instruction: jumps to the nth instruction if the int-valued register is negative */
  case class reljneg(n: Int, r: RegisterContainer) extends Instruction {
    override def next() = {
      if (getIntValue(r) < 0) goto(instructionNumber + n)
      else nextInstruction()
    }
  }

  /** label instruction: names a point in the code */
  case class label(name: String) extends Instruction {
    labels(name) = instructions.size // current line number in reading
  }

  /**
   * func instruction: names a function in the code
   *  Conventions:
   *   - r0 is caller saved (it is used for the return value)
   *   - all other registers are callee saved
   */
  case class func(name: String, stackSpace: Int = 0) extends Instruction {
    functions(name) = instructions.size // current line number in reading
    override def next() = {
      // go to next ret in the code
      while (!instructions(instructionNumber).isInstanceOf[ret])
        instructionNumber += 1
      nextInstruction()
    }
  }

  /** call instruction */
  case class call(name: String) extends Instruction {
    override def action() = {
      // put return address on stack
      mov(instructionNumber, tmpRegister).action()
      inc(rsp).action()
      store(tmpRegister, getIntValue(rsp)).action()

      // function prologue:
      push(rbp).action()
      mov(rsp, rbp).action()
    }
    override def next() = goto(functions(name))
  }

  /** ret instruction */
  case class ret extends Instruction {
    override def action() = {
      // function epilogue:
      mov(rbp, rsp).action()
      pop(rbp).action()

      // put return addres in tmpRegister
      load(getIntValue(rsp), tmpRegister).action()
      dec(rsp).action()
    }
    override def next() = goto(getIntValue(tmpRegister) + 1)
  }
  
  /** implicit conversion that allows jumps to labels */
  implicit def label2Line(labelName: String): Int = {
    try {
      labels(labelName)
    } catch {
      case e: NoSuchElementException => {
        // picking a magic negative number for the ID, when we see a goto(n) 
        // where n<0 we will know to check the unresolvedLabels map for 
        // the correct line number.
        val ID: Int = -unresolvedLabels.size - 1
        unresolvedLabels(ID) = labelName
        return ID
      }
    }
  }

  /** kill instruction: ends program execution */
  object kill extends Instruction {
    override def next() = {}
  }

  /** Internal helper function for int operations */
  protected def getIntValue(r: RegisterContainer): Int = {
    val reg = r.getRegister()
    if (reg.isInstanceOf[IntRegister])
      return reg.asInstanceOf[IntRegister].value
    else
      runErr(reg + " does not contain an integer.")
  }

  /** Internal helper function for double operations */
  protected def getDoubleValue(r: RegisterContainer): Double = {
    val reg: Register = r.getRegister
    if (reg.isInstanceOf[DoubleRegister])
      return reg.asInstanceOf[DoubleRegister].value
    else
      runErr(r + " does not contain a floating point value.")
  }

  /** iprint instruction: prints integer in register */
  case class iprint(r: RegisterContainer) extends Instruction {
    override def action() = println(getIntValue(r))
  }

  /** fprint instruction: prints floating point value in register */
  case class fprint(r: RegisterContainer) extends Instruction {
    override def action() = println(getDoubleValue(r))
  }

  /** sprint instruction: prints an immediate string */
  case class sprint(string: String) extends Instruction {
    override def action() = println(string)
  }

  /** float2int instruction: truncates floating point value in r1 and puts the result in r2 */
  case class float2int(r1: RegisterContainer, r2: RegisterContainer) extends Instruction {
    override def action() = mov(getDoubleValue(r1).asInstanceOf[Int], r2).action()
  }

  /** int2float instruction: converts int value in r1 and puts the floating point result in r2 */
  case class int2float(r1: RegisterContainer, r2: RegisterContainer) extends Instruction {
    override def action() = mov(getIntValue(r1).asInstanceOf[Double], r2).action()
  }

  /** add instruction: adds integers from 2 registers and puts the result in r3 */
  case class add(r1: RegisterContainer, r2: RegisterContainer, r3: RegisterContainer) extends Instruction {
    override def action() = mov(getIntValue(r1) + getIntValue(r2), r3).action()
  }

  /* Helpers to let adding an immediate translate to moving to a scratch register and adding */
  def add(x: Int, r2: RegisterContainer, r3: RegisterContainer): Unit = {
    mov(x, tmpRegister)
    add(tmpRegister, r2, r3)
  }
  def add(r1: RegisterContainer, x: Int, r3: RegisterContainer): Unit = {
    mov(x, tmpRegister)
    add(r1, tmpRegister, r3)
  }

  /* sub instruction: subtracts integers from 2 registers and puts the result in r3 */
  case class sub(r1: RegisterContainer, r2: RegisterContainer, r3: RegisterContainer) extends Instruction {
    override def action() = mov(getIntValue(r1) - getIntValue(r2), r3).action()
  }

  /** mul instruction: multiplies integers from 2 registers and puts the result in r3 */
  case class mul(r1: RegisterContainer, r2: RegisterContainer, r3: RegisterContainer) extends Instruction {
    override def action() = mov(getIntValue(r1) * getIntValue(r2), r3).action()
  }

  /** idiv instruction: divides integers from 2 registers and puts the result in r3 */
  case class div(r1: RegisterContainer, r2: RegisterContainer, r3: RegisterContainer) extends Instruction {
    override def action() = mov(getIntValue(r1) / getIntValue(r2), r3).action()
  }

  /** mod instruction: takes the modulus integers from 2 registers and puts the result in r3 */
  case class mod(r1: RegisterContainer, r2: RegisterContainer, r3: RegisterContainer) extends Instruction {
    override def action() = mov(getIntValue(r1) % getIntValue(r2), r3).action()
  }

  /** inc instruction: increments the integer value from a register by 1 */
  case class inc(r: RegisterContainer) extends Instruction {
    override def action() = mov(getIntValue(r) + 1, r).action()
  }

  /** dec instruction: decrements the integer value from a register by 1 */
  case class dec(r: RegisterContainer) extends Instruction {
    override def action() = mov(getIntValue(r) - 1, r).action()
  }

  /** fadd instruction: adds floating point numbers from 2 registers and puts the result in r3 */
  case class fadd(r1: RegisterContainer, r2: RegisterContainer, r3: RegisterContainer) extends Instruction {
    override def action() = mov(getDoubleValue(r1) + getDoubleValue(r2), r3).action()
  }

  /** fsub instruction: subtracts floating point numbers from 2 registers and puts the result in r3 */
  case class fsub(r1: RegisterContainer, r2: RegisterContainer, r3: RegisterContainer) extends Instruction {
    override def action() = mov(getDoubleValue(r1) - getDoubleValue(r2), r3).action()
  }

  /** fmul instruction: adds floating point numbers from 2 registers and puts the result in r3 */
  case class fmul(r1: RegisterContainer, r2: RegisterContainer, r3: RegisterContainer) extends Instruction {
    override def action() = mov(getDoubleValue(r1) * getDoubleValue(r2), r3).action()
  }

  /** fdiv instruction: adds floating point numbers from 2 registers and puts the result in r3 */
  case class fdiv(r1: RegisterContainer, r2: RegisterContainer, r3: RegisterContainer) extends Instruction {
    override def action() = mov(getDoubleValue(r1) / getDoubleValue(r2), r3).action()
  }

  /* Internal exceptions methods for readtime/runtime errors */
  private def readErr(str: String) = {
    val errStr = ("Syntax error at instruction " + (instructions.size + 1)
      + " after " + instructions.last + ", " + str)
    throw new RuntimeException("\n" + errStr)
  }
  private def runErr(str: String) = {
    val errStr = ("Runtime error at instruction " + instructionNumber
      + " = " + instructions(instructionNumber) + ", " + str)
    throw new RuntimeException(errStr)
  }

  /* Internal debug/testing/display functions. */
  protected def registerInfo(r: Container): String = {
    val reg: Register = r.getRegister
    val titleStr = "Register " + r + " info:"
    val infoStr = {
      reg match {
        case IntRegister(data) => "Type=Int, Data=" + data
        case DoubleRegister(data) => "Type=Double, Data=" + data
        case _ => "Unhandled Register type"
      }
    }
    return titleStr + "\n" + infoStr
  }

  protected def printRegisterInfo(r: Container): Unit = {
    println(registerInfo(r))
  }

}
