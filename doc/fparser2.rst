..  Copyright (c) 2017-2019 Science and Technology Facilities Council.

    All rights reserved.

    Modifications made as part of the fparser project are distributed
    under the following license:

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

    1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

    3. Neither the name of the copyright holder nor the names of its
    contributors may be used to endorse or promote products derived from
    this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

.. _fparser2 :

fparser2
========

Fparser2 provides support for parsing Fortran up to and including
Fortran 2003. This is implemented in the Fortran2003.py `file`__ and
contains an entirely separate parser to fparser1 that includes rules
for Fortran 2003 syntax. Support for Fortran 2008 is being added in
the Fortran2008.py `file`__ which extends the Fortran2003 rules
appropriately. At this time Fortran2008 support is limited to
submodules.

__ https://github.com/stfc/fparser/blob/master/src/fparser/two/Fortran2003.py
__ https://github.com/stfc/fparser/blob/master/src/fparser/two/Fortran2008.py


Getting Going : Script
----------------------

fparser2 can be run from the command line by using the `fparser2.py`
script located in the `scripts` directory. One or more input files can
be provided. These files are parsed in turn and the parsed Fortran is output
to the screen (most likely with a different formatting to the input as
fparser2 does not preserve format), or an appropriate error is output.
::

   >>> cat simple.f90
   program simple
   end
   >>> src/fparser/scripts/fparser2.py simple.f90
   PROGRAM simple
   END PROGRAM simple
   >>> cat error.f90
   prog error
   end
   >>> src/fparser/scripts/fparser2.py error.f90
   Syntax error: at line 1
   >>>prog error

   parsing 'src/error.f90' failed at line #1'prog error'
   started at line #1'prog error'


Getting Going : Python
----------------------

As with the other parser (:ref:`fparser`), the source code to parse
must be provided via a Fortran reader which is an instance of either
`FortranFileReader` or `FortranStringReader` (see
:ref:`readfortran`).

The required parser is then created using an instance of the
`ParserFactory` class. `ParserFactory` either returns a
Fortran2003-compliant parser or a Fortran2008-compliant parser
depending on the `std` argument provided to its create method.

Finally the parser is provided with the Fortran reader and returns an
abstract representation (a parse-tree) of the code,
if the code is valid Fortran. This parse-tree can be output as Fortran by
printing it. The parse-tree hierarchy can also be output in textual form by
executing it. For example:

::
   
    >>> from fparser.two.parser import ParserFactory
    >>> from fparser.common.readfortran import FortranFileReader
    >>> reader = FortranFileReader("compute_unew_mod.f90",
                                   ignore_comments=False)
    >>> f2008_parser = ParserFactory().create(std="f2008")
    >>> parse_tree = f2008_parser(reader)
    >>> print parse_tree
    MODULE compute_unew_mod
      USE :: kind_params_mod
      USE :: kernel_mod
      USE :: argument_mod
      USE :: grid_mod
      USE :: field_mod
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: invoke_compute_unew
      PUBLIC :: compute_unew, compute_unew_code
      TYPE, EXTENDS(kernel_type) :: compute_unew
      ...
    >>> ast
    Program(Module(Module_Stmt('MODULE', Name('compute_unew_mod')),Spec
    ification_Part(Use_Stmt(None, Name('kind_params_mod'), '', None),Us
    e_Stmt(None, Name('kernel_mod'), '', None),Use_Stmt(None, Name('arg
    ument_mod'), '', None),Use_Stmt(None, Name('grid_mod'), '', None),U
    se_Stmt(None, Name('field_mod'), '', None),Implicit_Part(Implicit_S
    tmt('NONE')), Access_Stmt('PRIVATE', None),Access_Stmt('PUBLIC', Na
    me('invoke_compute_unew')),Access_Stmt('PUBLIC', Access_Id_List(','
    , (Name('compute_unew'),Name('compute_unew_code')))),Derived_Type_D
    ef(Derived_Type_Stmt(Type_Attr_Spec('EXTENDS',Name('kernel_type')),
    Type_Name('compute_unew'), None), ...

Note that the two readers will ignore (and dispose of) comments by
default. If you wish comments to be retained then you must set
`ignore_comments=False` when creating the reader. The AST created by
fparser2 will then have `Comment` nodes representing any comments
found in the code. Nodes representing in-line comments will be added
immediately following the node representing the code in which they
were encountered.

Note that empty input, or input that consists of purely white space
and/or newlines, is not treated as invalid Fortran and an empty AST is
returned. Whilst this is not strictly valid, most compilers have this
behaviour so we follow their lead.

If the code is invalid Fortran then a `FortranSyntaxError` exception
will be raised which indicates the offending line of code and its line
number. For example:

::
   
   >>> from fparser.common.readfortran import FortranStringReader
   >>> code = "program test\nen"
   >>> reader = FortranStringReader(code)
   >>> from fparser.two.parser import ParserFactory
   >>> f2008_parser = ParserFactory().create(std="f2008")
   >>> ast = f2008_parser(reader)
   Traceback (most recent call last):
     File "<stdin>", line 1, in <module>
     File "fparser/two/Fortran2003.py", line 1300, in __new__
       raise FortranSyntaxError(error)
   fparser.two.Fortran2003.FortranSyntaxError: at line 2
   >>>en

Unsupported Features
--------------------

Statement Functions
+++++++++++++++++++

Fparser2 is currently not able to distinguish between statement
functions and array assignments when one or more array assignment
statements are the first statements after a declaration section. This
limitation leads to these particular array assignments being
incorrectly parsed as statement functions.

To avoid this incorrect behaviour, support for statement functions has
been temporarily removed from fparser2. However, with this change,
statement functions will be incorrectly parsed as array assignments
when one or more statement function statements are the last statements
in a declaration section.

Whilst any incorrect behaviour should be avoided, the behaviour of
this temporary change is considered preferable to the former case, as
array assigments are more common than statement functions.

Extensions
----------

Many compilers support extensions to standard Fortran and codes often
make use of them. This section documents the extensions to the
standard that are supported by fparser2.

Cray Pointers
+++++++++++++

Cray pointers are part of a non-standard extension that provides a
C-like pointer in Fortran. This is accomplished through a pair of
variables: an integer "pointer" that holds a memory address, and a
"pointee" that is used to dereference the pointer. For example::

  pointer (my_pointer, my_pointee)

For a specification and a more detailed explanation see
http://pubs.cray.com/content/S-3901/8.6/cray-fortran-reference-manual-s-3901-86/types
or https://gcc.gnu.org/onlinedocs/gfortran/Cray-pointers.html.


X Format
++++++++

An X edit descriptor in a format statement specifies the position
(forward from the current position) at which the next character will
be transmitted to or from a record. In standard Fortran2003 the X edit
descriptor must be preceeded by an integer which specifies how far
forward from the current position. The 'x-format' extension allows the
X edit descriptor to be specified without a preceeding integer. When
omitted, the value is implicitly assumed to be one. For example::

  100 format (x)

For more information see
https://gcc.gnu.org/onlinedocs/gfortran/X-format-descriptor-without-count-field.html

Hollerith Constant
++++++++++++++++++

A Hollerith constant is a way of specifying a string as a sequence of
characters preceded by the string length and separated by an 'H'. For
example::
  
  5Hhello
  11Hhello there

Hollerith constants were used in Fortran before character strings were
introduced in Fortran 77. In Fortran 77 the use of Hollerith constants
was deprecated and in Fortran 95 they were removed from the
language. However, many compilers still support Hollerith constants
for legacy Fortran code.
  
The 'hollerith' extension adds support in fparser for Hollerith
constants. This support is currently limited to those specified in
format statements. There is currently no support for 1) constants in
DATA statements and 2) constant actual arguments in subroutine CALL
statements (which are its other uses as specified in the Fortran 66
standard).

For more information see
https://gcc.gnu.org/onlinedocs/gfortran/Hollerith-constants-support.html
or https://en.wikipedia.org/wiki/Hollerith_constant

Dollar Descriptor
+++++++++++++++++

A dollar descriptor is used to specify that the carriage return in a
write statement should be suppressed. For example::

  100 FORMAT ('Enter your name ',$)

This extension is not specified in the Fortran standard but is
implemented by a number of compilers. The 'dollar-descriptor'
extension adds support for the dollar descriptor in fparser.

For more information see
https://software.intel.com/en-us/fortran-compiler-developer-guide-and-reference-dollar-sign-and-backslash-editing

Classes
-------

.. autoclass:: fparser.common.readfortran.FortranFileReader
    :members:


.. autoclass:: fparser.common.readfortran.FortranStringReader
    :members:


.. autoclass:: fparser.two.parser.ParserFactory
    :members:

Includes
--------

Fortran supports the `include` statement in order to inline code from
other locations into the current file.

The interpretation of where the include content comes from is defined
as being processor dependent in the Fortran standard. Fparser assumes
that the content comes from another another file. For example ::

  program x
  include 'content.inc'
  end program

implies that ``content.inc`` is a file containing Fortran code. For
example ::

  print *, "Hello"

The above example would then be equivalent to the following Fortran
code ::

  program x
  print *, "Hello"
  end program

The Fortran standard specifies that the `include` statement is not a
Fortran statement, rather it indicates that when the code is processed
the contents of the `include` file should replace the include
statement as the code is parsed.

fparser supports `include` statements in its reader classes
(`FortranStringReader` and `FortranFileReader`), so the parser itself
need not be aware of the original `include` statements.

The directory locations in which to search for `include` files can be
specified to a reader class instance via the optional `include_dirs`
argument, which should be iterable. For example::

  reader = FortranFileReader(my_file, include_dirs=["dir1", "dir2"])

If a relative directory is specified then the location is with respect
to the input file.  The reader class instance will search in the
specified directory order and include the contents of the first
matching file found.

If no include_dirs argument is supplied then the default search
location is ``['.']``. Therefore include files would need to be in the
same directory as the input file for them to be found.

.. note:: At the moment it is not possible to specify include
          directories in the fparser2 script.

In a compiler, all include files must be found otherwise there is an
error. However, with a code parser this is not necessarily the case. A
user might not want to include all files when parsing, perhaps for
simplicity, or perhaps because the files are not readily available. To
support this, fparser has been extended to recognise `include`
statements as part of the parse tree. If a particular `include` file
is not found then the associated `include` statement will be parsed
like a standard Fortran statement. If we again consider the simple
example from earlier::

  program x
  include 'content.inc'
  end program

with ``content.inc`` containing the following Fortran code::

  print *, "Hello"

then if the ``content.inc`` file is found the output of fparser
would be::

  PROGRAM x
  PRINT *, "Hello"
  END PROGRAM x

but if the ``content.inc`` file is not found then the output of
fparser would be::

  PROGRAM x
  INCLUDE 'content.inc'  
  END PROGRAM x
  
Clearly in the latter case the parser does require the code to be
valid with the `include` statement in it. For example, assuming the
content of ``endprogram.inc`` is ``end program`` then the following
example would be parsed successfully if the `endprogram.inc` include
file was found but would fail if the include file was not found::

  program x
  include 'endprogram.inc'

Walking the AST
---------------

fparser2 provides two functions to support the traversal of the
AST that it constructs:

.. automethod:: fparser.two.utils.walk_ast

.. automethod:: fparser.two.utils.get_child

