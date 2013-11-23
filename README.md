# Pike

Pike is a small Scala DSL that mimics an Assembly-like language.

Here is an example program:

```scala
object SquareSummer extends Pike {
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

This program computes 10^2 + 9^2 + ... + 1^2 = 385 and prints the result.



### How to use Pike

The simplest way is with SBT.

```
  git clone https://github.com/splewis/Pike.git
  cd Pike/
  sbt test
```

You can also import the project to eclipse if you have an updated Scala IDE plugin installed.
