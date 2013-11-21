Pike
========================

Pike is a small Scala DSL that mimics an Assembly-like language.

A feature of Pike is the type safety of the runtime system. For example, the following commands result in an error:
mov(1.3, "r1") // floating point store
mov(1, "r2")   // integer store
add("r1", "r2", "r3")

In addition, exception output indicates what went wrong.
