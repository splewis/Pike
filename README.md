# Pike

Pike is a small Scala DSL that mimics an Assembly-like language.

Here are a few versions of a simple program that computes the sum
10^2 + 9^2 + ... + 1^2 = 385 and prints the result.

```scala
object SquareSummer1 extends Pike {
  def main(args: Array[String]) = {
    mov(0, r0)  // summation register
    mov(10, r1) // temp register for i=[10, 9, ..., 1, 0]
    mov(0, r2)  // temp register for squares of r1

    mov(r1, r2)     // relative jumps lands here
    mul(r2, r2, r2) // r2 * r2 -> r2
    add(r0, r2, r0) // r0 + r2 -> r0
    dec(r1)         // decrement r1
    reljpos(-4, r1) // jumps back 4 instructions if r1 is positive

    iprint(r0)      // prints integer value in r0
    run
  }
}
```


```scala
// We can name points in the code using labels, making jumps easier to use

object SquareSummer2 extends Pike {
  def main(args: Array[String]) = {
    mov(0, r0)  // summation register
    mov(10, r1) // temp register for i=[10, 9, ..., 1, 0]
    mov(0, r2)  // temp register for squares of r1

    label("loop")
    mov(r1, r2)
    mul(r2, r2, r2) // r2 * r2 -> r2
    add(r0, r2, r0) // r0 + r2 -> r0
    dec(r1)
    jpos("loop", r1) // jumps to loop label if r1 is positive

    iprint(r0)       // prints integer value in r0
    run
  }
}
```


```scala
// We can also add functions to the code
// Arguments are passed on the stack and the return value is in r0

object SquareSummer3 extends Pike {
  def main(args: Array[String]) = {

    func("square")    // this defines the function square
    loadstack(-1, r0) // reads 1 value above the rbp (gets the function parameter)
    mul(r0, r0, r0)
    ret()             // this ends the definition of the square function

    mov(0, r1)  // summation register
    mov(10, r2) // temp register for i=[1..10]

    label("loop")
    push(r2) // pushing the argument to the square function
    call("square")
    pop(r2)
    add(r0, r1, r1)
    dec(r2)
    jpos("loop", r2)

    iprint(r1)
    run
  }
}
```


### Language Features

Some things implemented in the language:

- 10 general purpose registers (r0 to r9)
- Arithmetic on integer/floating point values
- Jumps, conditional jumps, branching
- Labels (and jumps to them)
- Defining functions and calling them (stack-based execution, the call and ret instructions for functions internally set rsp and rbp registers)

Some things that would make it more interesting:

- Some way to include other files, or at least functions defined elsewhere


### How to use Pike

The simplest way is with SBT.

```
  git clone https://github.com/splewis/Pike.git
  cd Pike/
  sbt test
```

The command ```sbt package``` will produce a jar that you can add to the classpath to compile with.

You can also import the project to eclipse if you have an updated Scala IDE plugin installed.


### Docs

The autogenerated docs are useful for seeing what the language can do. A recent version should be
hosted at <http://www.cs.utexas.edu/~splewis/PikeDocs/#Pike.Pike> or can be built running
```sbt doc```
