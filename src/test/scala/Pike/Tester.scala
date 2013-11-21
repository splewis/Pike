package Pike

object Tester extends Pike {
  def main(args: Array[String]): Unit = {
    mov(15, "r1")
    mov(23, "r2")
    mov(1, "r3")
    add("r1", "r2", "r3")
    inc("r3")

    mov(1.5, "r4")
    mov(1.25, "r5")
    fadd("r4", "r5", "r6")
    
    
    printRegisterInfo("r1")
    printRegisterInfo("r2")
    printRegisterInfo("r3")
    printRegisterInfo("r4")
    printRegisterInfo("r5")
    printRegisterInfo("r6")
    
  }
}
