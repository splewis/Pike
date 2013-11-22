package Pike

import java.util.IllegalFormatException
import scala.collection.mutable.MutableList

class Pike {

  private val NumRegisters: Int = 16
  private val registers = new Array[Register](NumRegisters)

  private var shouldKill = false
  private var instructionNumber = 0
  private var instructions = new MutableList[Instruction]()

  abstract sealed class Register
  case class IntRegister(value: Int) extends Register
  case class DoubleRegister(value: Double) extends Register
  case class StringRegister(value: String) extends Register

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

  private def nextInstruction(): Unit = {
    goto(instructionNumber + 1)
  }

  abstract class Instruction {
    instructions += this
    def action(): Unit
    def apply() = action()
  }

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

  case class jmp(n: Int) extends Instruction {
    override def action() = goto(n)
  }

  private def goto(n: Int): Unit = {
    instructionNumber = n
    if (shouldKill || instructionNumber >= instructions.size || instructionNumber < 0)
      println("Program terminated")
    else
      instructions(instructionNumber).action()
  }

  def run(): Unit = instructions(0).action()

  object kill extends Instruction {
    override def action() = {} // i.e. do nothing
  }

  case class add(r1: String, r2: String, r3: String) extends Instruction {
    override def action() = {
      val reg1: Register = getRegister(r1)
      val reg2: Register = getRegister(r2)
      var err: Boolean = false

      if (reg1.isInstanceOf[IntRegister] && reg2.isInstanceOf[IntRegister]) {
        val sum: Int = reg1.asInstanceOf[IntRegister].value + reg2.asInstanceOf[IntRegister].value
        val inst: Instruction = mov(sum, r3)
        inst.action()
      } else {
        throw new RuntimeException("Type mismatch integer add" + r1 + " to " + r2)
      }
    }

  }

  //    private def binaryIntFunction(r1: String, r2: String, r3: String, f: (Int, Int) => Int): Unit = {
  //      val reg1: Register = getRegister(r1)
  //      val reg2: Register = getRegister(r2)
  //      var err: Boolean = false
  //  
  //      if (reg1.isInstanceOf[IntRegister]) {
  //        if (reg2.isInstanceOf[IntRegister])
  //          _mov(f(reg1.asInstanceOf[IntRegister].value, reg2.asInstanceOf[IntRegister].value), r3)
  //        else
  //          err = true
  //      } else {
  //        err = true
  //      }
  //      if (err)
  //        throw new RuntimeException("Type mismatch integer add" + r1 + " to " + r2)
  //    }

  //    private def _add(r1: String, r2: String, r3: String): Unit = {
  //      binaryIntFunction(r1, r2, r3, (a, b) => a + b)
  //    }
  //
  //  private def _mod(r1: String, r2: String, r3: String): Unit = {
  //    binaryIntFunction(r1, r2, r3, (a, b) => a % b)
  //  }
  //
  //  private def _mul(r1: String, r2: String, r3: String): Unit = {
  //    binaryIntFunction(r1, r2, r3, (a, b) => a * b)
  //  }
  //
  //  private def _idiv(r1: String, r2: String, r3: String): Unit = {
  //    binaryIntFunction(r1, r2, r3, (a, b) => a / b)
  //  }
  //
  //  private def binaryFloatFunction(r1: String, r2: String, r3: String, f: (Double, Double) => Double): Unit = {
  //    val reg1: Register = getRegister(r1)
  //    val reg2: Register = getRegister(r2)
  //    var err: Boolean = false
  //
  //    if (reg1.isInstanceOf[DoubleRegister]) {
  //      if (reg2.isInstanceOf[DoubleRegister])
  //        _mov(f(reg1.asInstanceOf[DoubleRegister].value, reg2.asInstanceOf[DoubleRegister].value), r3)
  //      else
  //        err = true
  //
  //    } else {
  //      err = true
  //    }
  //
  //    if (err)
  //      throw new RuntimeException("Type mismatch floating add " + r1 + " to " + r2)
  //  }
  //
  //  private def _fadd(r1: String, r2: String, r3: String): Unit = {
  //    binaryIntFunction(r1, r2, r3, (a, b) => a + b)
  //  }
  //
  //  private def _fmul(r1: String, r2: String, r3: String): Unit = {
  //    binaryIntFunction(r1, r2, r3, (a, b) => a * b)
  //  }
  //
  //  private def _fidiv(r1: String, r2: String, r3: String): Unit = {
  //    binaryIntFunction(r1, r2, r3, (a, b) => a / b)
  //  }
  //
  //  private def _inc(rName: String): Unit = {
  //    val reg = getRegister(rName)
  //    if (reg.isInstanceOf[IntRegister])
  //      _mov(reg.asInstanceOf[IntRegister].value + 1, rName)
  //    else
  //      throw new RuntimeException("Cannot increment non-Int register " + rName)
  //  }

  def registerInfo(rName: String): String = {
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

  def printRegisterInfo(rName: String): Unit = {
    println(registerInfo(rName))
  }

}
