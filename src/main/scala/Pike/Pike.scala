package Pike

import java.util.IllegalFormatException

class Pike {

  val NumRegisters: Int = 16

  type Subroutine = (() => Unit)

  abstract sealed class Register
  case class IntRegister(value: Int) extends Register
  case class DoubleRegister(value: Double) extends Register
  case class StringRegister(value: String) extends Register

  private val registers = new Array[Register](NumRegisters)

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

  def mov(value: Any, rName: String): Unit = {
    var newReg: Register = null
    value match {
      case d: Int => newReg = new IntRegister(d)
      case d: Float => newReg = new DoubleRegister(d)
      case d: Double => newReg = new DoubleRegister(d)
      case d: String => newReg = new StringRegister(d)
      case _ => throw new RuntimeException("Illegal type put in " + rName);
    }
    registers(getRegisterIndex(rName)) = newReg
  }

  private def binaryIntFunction(r1: String, r2: String, r3: String, f: (Int, Int) => Int): Unit = {
    val reg1: Register = getRegister(r1)
    val reg2: Register = getRegister(r2)
    var err: Boolean = false

    if (reg1.isInstanceOf[IntRegister]) {
      if (reg2.isInstanceOf[IntRegister])
        mov(f(reg1.asInstanceOf[IntRegister].value, reg2.asInstanceOf[IntRegister].value), r3)
      else
        err = true
    } else {
      err = true
    }
    if (err)
      throw new RuntimeException("Type mismatch integer add" + r1 + " to " + r2)
  }

  def add(r1: String, r2: String, r3: String): Unit = {
    binaryIntFunction(r1, r2, r3, (a, b) => a + b)
  }

  def mod(r1: String, r2: String, r3: String): Unit = {
    binaryIntFunction(r1, r2, r3, (a, b) => a % b)
  }

  def mul(r1: String, r2: String, r3: String): Unit = {
    binaryIntFunction(r1, r2, r3, (a, b) => a * b)
  }

  def idiv(r1: String, r2: String, r3: String): Unit = {
    binaryIntFunction(r1, r2, r3, (a, b) => a / b)
  }

  def binaryFloatFunction(r1: String, r2: String, r3: String, f: (Double, Double) => Double): Unit = {
    val reg1: Register = getRegister(r1)
    val reg2: Register = getRegister(r2)
    var err: Boolean = false

    if (reg1.isInstanceOf[DoubleRegister]) {
      if (reg2.isInstanceOf[DoubleRegister])
        mov(f(reg1.asInstanceOf[DoubleRegister].value, reg2.asInstanceOf[DoubleRegister].value), r3)
      else
        err = true

    } else {
      err = true
    }

    if (err)
      throw new RuntimeException("Type mismatch floating add " + r1 + " to " + r2)
  }

  def fadd(r1: String, r2: String, r3: String): Unit = {
    binaryIntFunction(r1, r2, r3, (a, b) => a + b)
  }

  def fmul(r1: String, r2: String, r3: String): Unit = {
    binaryIntFunction(r1, r2, r3, (a, b) => a * b)
  }

  def fidiv(r1: String, r2: String, r3: String): Unit = {
    binaryIntFunction(r1, r2, r3, (a, b) => a / b)
  }

  def inc(rName: String): Unit = {
    val reg = getRegister(rName)
    if (reg.isInstanceOf[IntRegister])
      mov(reg.asInstanceOf[IntRegister].value + 1, rName)
    else
      throw new RuntimeException("Cannot increment non-Int register " + rName)
  }

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
