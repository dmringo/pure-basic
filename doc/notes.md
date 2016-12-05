# Notes on BASIC semantics

### Line number ordering

Line numbers are significant, not only for GOTO statements, but they literally
  specify the order of unconditional execution. For example,

~~~
10 PRINT "HELLO"
20 PRINT "WORLD"
15 PRINT "TO THE ENTIRE"
~~~

yields

~~~
> HELLO
> TO THE ENTIRE
> WORLD
~~~


### FOR - NEXT considerations:

A `NEXT` statement acts like a `GOTO` with finer target granularity and some
interesting side effects in memory.  It causes control flow to return to the
statement directly following the `FOR` loop it iterates.  This could be the next
line or on the same line as the `FOR` loop.  If care is not taken with the index
variable order (in n>1 dimensional looping) errors can occur:

~~~
50 for I = 1 to 10
60 for J = I to I + 3
70 print J
80 next I, J: REM this breaks once the I loop exits
~~~

This means that the variables that a `FOR` loop uses need to be marked as
exhausted once their limit is reached.  Producing a meaningful error message
would be nice too, e.g. "attempt to step to next value of exhausted variable I".
