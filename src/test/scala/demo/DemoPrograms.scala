package demo

import pike._

object DemoPrograms {
  def main(args: Array[String]): Unit = {
    //    SimpleDemo.prog()
    //    SimpleDemo.prog()
    //    SumSqDemo.prog()
    //    FunctionDemo.prog()
    //    PowDemo.prog()
    //    FactorialDemo.prog()
  }
}

object SimpleDemo extends Pike {
  def prog() = {
    mov(15, r0)
    mov(25, r1)
    add(r0, r1, r2)
    iprint(r2)
    run
  }
}

object SumSqDemo extends Pike {
  def prog() = {
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
    iprint(r0)
    run
  }
}

object FunctionDemo extends Pike {
  def prog() = {
    func("square")
    loadstack(-1, r0)
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
    iprint(r0)
    run
  }
}

object PowDemo extends Pike {
  def prog() = {
    loadStdLib
    sprintln("This program computes x^y for integer x and y")

    sprint("enter x: ")
    readint(r1)
    sprint("enter y: ")
    readint(r2)

    push(r2)
    push(r1)
    call("pow")
    pop(r1)
    pop(r2)

    sprint("result: ")
    iprint(r0)
    sprintln("")
    run
  }
}

object FactorialDemo extends Pike {
  def prog() = {
    func("factorial")
    push(r1)
    push(r2)
    loadstack(-1, r0) // x
    jz("base", r0)

    mov(r0, r1)
    mov(r0, r2)
    dec(r1) // x - 1
    push(r1)
    call("factorial") // fac(x-1) -> r0
    pop(r1)
    mul(r0, r2, r0) // r0*r2 = fac(x-1)*x -> r0
    jmp("end")

    label("base")
    mov(1, r0)

    label("end")
    pop(r2)
    pop(r1)
    ret()

    mov(7, r1)
    push(r1)
    call("factorial")
    pop(r1)
    iprint(r0)
    run

  }
}
