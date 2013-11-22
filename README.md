Pike
========================

Pike is a small Scala DSL that mimics an Assembly-like language.

Here is an example program:

```scala
	object SquareSummer extends Pike {
	  def main(args: Array[String]) = {
	    mov(0, r0) // summation register
	    mov(10, r1) // temp register for i=[1..10]
	    mov(0, r2) // temp register for i=[1..10]
	    label("loop")
	    mov(r1, r2)
	    mul(r2, r2, r2) // r2 * r2 -> r2
	    add(r0, r2, r0) // r0 + r2 -> r2
	    dec(r1)
	    jpos("loop", r1) // jumps to loop label if r1 is positive
	    iprint(r0) // prints integer value in r0
	    run
	  }
	}
```

This program computes 10^2 + 9^2 + ... + 1^2 = 385 and prints the result.



