#GCC中常见Message
---
1. warning:suggest parentheses around assignment used as truth value 
---
```
  if ( atype=search(alphatp, 1) )
```
---
```
在C语言中，非0代表TRUE，反之为FALSE。atype值是用于最后的判断用的。
但是由于长期的编程实践告诉我们，人们经常在"="和“==”的使用上出现手误，
所以gcc编译器为此要求我们明确地告诉编译器它是"="而不是"=="，是故意，而非手误。
```
---
###解决
if ( ( atype=search(alphatp, 1) ) )  即加一个括号括起来就可以了

2. clang: error: unknown argument: '-fexec-charset=UTF-8'
---
###解决
```
$ sudo vim /usr/share/GNUstep/Makefiles/Additional/base.make
> #AUXILIARY_OBJCFLAGS += -fexec-charset=UTF-8
```
