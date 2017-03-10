.class MyClass
.super java/lang/Object


.method static even(II)I
  .limit stack 5
  .limit locals 5
     iload 0
     iload 1
     sipush 1
     iadd
     iadd
     ireturn
.end method

