<h1>numpy.f2py FAQ</h1>


# Wrapping Fortran codes #

## How to prevent Fortran STOP to kill Python? ##

_Problem:_ Some Fortran codes use STOP statement to exit a Fortran program. However, when such Fortran codes are wrapped to Python, STOP will kill also Python program that called the corresponding Fortran routine.

_Example:_ Let's consider the following Fortran code:
```
!File: foo.f90
subroutine foo(a)
  integer a
  if (a.gt.10) then
    print*, "Fortran: a>10 is bad, stopping here."
    stop(10)
  end if
  print*,"Fortran foo: a<=10 is good, continue."
end subroutine foo
```
and wrap it with f2py as follows:
```
f2py -c -m m foo.f90
```
that will create a Python extension module `m.so`.
In Python:
```
>>> import m
>>> m.foo(5)
 Fortran foo: a<=10 is good, continue.
>>> m.foo(15)
 Fortran: a>10 is bad, stopping here.
STOP 10
```
Note that after calling `m.foo(15)` Python exits.

### Solution 1 - compiler independent, but requires minor editing of Fortran sources ###

This solution is based on avoiding Fortran STOP statement execution with a call to user-defined function that is placed before every STOP statement and that will handle graceful exit from a Fortran code. Modification of STOP statements can be achieved either manually or by writing a simple script that walks through Fortran sources and performs the necessary changes _in-situ_.

Let's consider the example Fortran code introduced above and change it as follows (`STOP <status>` statement is preceded with `CALL f2pystop(<status>)`):
```
!File: foo2.f90
subroutine foo2(a)
  integer a
  if (a.gt.10) then
    print*, "Fortran: a>10 is bad, stopping here."
    call f2pystop(10) ! f2pystop must raise an exception that will trigger a long jump to the corresponding wrapper function
    stop(10)
  end if
  print*,"Fortran foo: a<=10 is good, continue."
end subroutine foo2
```
Now generate a signature file with
```
f2py -m m2 -h m2.pyf foo2.f90
```
and modify it as follows (add the four lines defining f2pystop signature)
```
python module m2 ! in 
    interface  ! in :m2
        subroutine foo2(a) ! in :m2:foo.f90
            integer :: a
            intent (callback) f2pystop
            external f2pystop
            integer status
            call f2pystop(status)
        end subroutine foo2
    end interface 
end python module m2
```

_Note:_ An alternative to generating a signature file would be to add the following comment lines just before the first STOP statement of each function in a Fortran source file:
```
!intent(callback) f2pystop
!external f2pystop
```

Now generate an extension module with
```
f2py -c m2.pyf foo2.f90
```
and in Python derive F2PYSTOP class from Exception:
```
class F2PYSTOP(Exception):
    def __call__(self, status):
        raise self.__class__(status)
```
that will handle a jump from Fortran STOP statement to the calling Python process:
```
>>> import m2
>>> class F2PYSTOP(Exception):
...     def __call__(self, status):
...         raise self.__class__(status)
... 
>>> m2.foo2(5, F2PYSTOP())
 Fortran foo: a<=10 is good, continue.
>>> m2.foo2(15, F2PYSTOP())
 Fortran: a>10 is bad, stopping here.
capi_return is NULL
Call-back cb_f2pystop_in_foo2__user__routines failed.
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 3, in __call__
__main__.F2PYSTOP: 10
>>> 
```
Now it is up to an user how to handle this exception.

_Note:_ Alternatively, one can use any callable object instead of the used F2PYSTOP instance as long as this will trigger an exception. Otherwise, the Fortran program will continue execution and hit the Fortran STOP statement that will kill Python.

### Remarks ###

See also [Issue 39](https://code.google.com/p/f2py/issues/detail?id=39) that discusses other options.


# Usage of extension modules #

## Multiple copies of extension module ##

_Problem:_ Wrapping of Fortran codes that use global data such as common blocks or module data, cannot be used recursively from Python because the global data is shared by the wrapped routines.

This problem has few solutions:
  1. For the same Fortran code, create extension modules with different names. See  [f2py-ml/2289](http://cens.ioc.ee/pipermail/f2py-users/2011-December/002289.html) for an example. The problem with this approach is that the depth of recursion is limited with the number of extension modules.
  1. Prior the subsequent import of an extension module, copy its file to unique location and import it from there. See [f2py-ml/921](http://cens.ioc.ee/pipermail/f2py-users/2004-September/000921.html), [f2py-ml/2290](http://cens.ioc.ee/pipermail/f2py-users/2011-December/002290.html) that also provide the import code. _Recommended solution_.