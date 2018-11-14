..  Copyright (c) 2017-2018 Science and Technology Facilities Council.

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

.. _developers:

Developers' Guide
=================

Reading Fortran
---------------

A key part of the fparser package is support for reading Fortran code.
`fparser.common.readfortran.FortranFileReader` provides this functionality
for source files while `FortranStringReader` supports Fortran source
provided as a string. Both of these classes sub-class `FortranReaderBase`:

.. autoclass:: fparser.common.readfortran.FortranReaderBase

Note that the setting for `ignore_comments` provided here can be overridden
on a per-call basis by methods such as `get_single_line`.

Fparser2
--------

Fparser2 supports Fortran2003 and is being extended to support
Fortran2008. It is a little less mature than fparser but is being
actively developed and is planned to replace fparser in the near
future.

.. _rules:

Rules
+++++

Each version of the Fortran language is defined as a set of rules in a
specification document. The Fortran2003 rules are specified here
https://wg5-fortran.org/N1601-N1650/N1601.pdf and the Fortran2008
rules are specified here
https://j3-fortran.org/doc/year/10/10-007r1.pdf.

Each rule has a number, for example the Fortran2003 document includes
the following top level rules `R201` and `R202`
::

    R201 program is program-unit
                    [ program-unit ] ...

    R202 program-unit is main-program
                         or external-subprogram
                         or module
                         or block-data

It can be seen that the right hand side of these rules consist of more
rules. Note, `[]` means that the content is optional. At some point in
the rule hierarchy rules start to be defined by text. For example,
taking a look at the specification of a module
::

    R1104 module is module-stmt
                    [ specification-part ]
                    [ module-subprogram-part ]
                    end-module-stmt

    R1105 module-stmt is MODULE module-name

    R1106 end-module-stmt is END [ MODULE [ module-name ] ]

it can be seen that rules `R1105` and `R1106` specify the actual code to
write e.g. `MODULE`. Here `module-name` is a type of `name` which has
a rule specifying what is valid syntax (see the specification document
for more details).

Therefore Fortran is specified as rules which reference other rules,
or specify a particular syntax. The top level rule of this hierarchy
is rule `R201`, which defines a program, see above.


Classes
+++++++

In fparser2 each rule is implemented in a class with the class names
closely following the rule names. For example, `program` is
implemented by the `Program` class and `program-unit` is implemented
by the `Program_Unit` class. In general, the name of the class
corresponding to a given rule can be obtained by replacing '-' with
'_' and capitalising each word.

The Fortran2003 classes exist in the Fortran2003.py file and the
Fortran2008 classes exist in the Fortran2008.py file (see
:ref:`Fortran2008` section for Fortran2008-specific implementation
details).

The Fortran2003 and Fortran2008 classes can inherit from a set of
pre-existing base classes which implement certain rule patterns in a
generic way. The base classes are contained in the `utils.py` file.

The base classes and rule patterns are discussed more in the
:ref:`base-classes` section.

The primary components of classes i.e. the parts that developers
typically need to be concerned with are:

1) the `subclass_names` list
2) the `use_names` list
3) the static `match` method
4) the `tostr` method

A `subclass_names` list of classes should be provided when the rule is
a simple choice between classes. In this case the `Base` class ensures that each
child class is tested for a match and the one that matches is
returned. An example of a simple choice rule is `R202`. See the
:ref:`program-unit-class` section for a description of its
implementation.

.. note::

   A `use_names` description, explanation and example needs to be added.

When the rule is not a simple choice the developer needs to supply a
static `match` method. An example of this is rule `R201`. See the
:ref:`program-class` section for a description of its implementation.

.. note::

   A `tostr` description, explanation and example needs to be added.

Class Relationships
+++++++++++++++++++

When a rule is a simple choice, the class implementing this rule
provides a list of classes to be matched in the `subclass_names` list
(or potentially `use_names` list). These class names are provided as
strings, not references to the classes themselves.

In fparser2 these strings are used to create class references to allow
matching to be performed. The creation of class references is
implemented by the `create` method of the `ParserFactory` object.

The `create` method of the `ParserFactory` class also links to
appropriate classes to create parsers compliant to the specified
standard.

.. note::

   The ParserFactory implementation needs to be explained.

A parser conforming to a particular Fortran standard is created by a
ParserFactory object. For example::

    >>> from fparser.two.parser import ParserFactory
    >>> parser_f2003 = ParserFactory().create(std="f2003")

The `create` method returns a `Program` *class* (called `parser_f2003`
in the above example) which contains a `subclasses` dictionary
(declared in its base class - called `Base`) configured with *all* the
Fortran2003 class relationships specified by the `subclass_names` and
`use_names` lists in each class.

As all classes inherit from the `Base` class, the `subclasses`
dictionary is available to all classes. If, for example, we query the
dictionary for the `Program` class relationships we get an empty list
as it has no `subclass_names` or `use_names` entries specified (see
:ref:`program-class`). If however, we query the dictionary for the
`Program_unit` relationships we get the list of classes specified in
that classes `subclass_names` list (see :ref:`program-unit-class`)::

    >>> parser_f2003.__name__
    'Program'
    >>> parser_f2003.subclasses['Program']
    []
    >>> parser_f2003.subclasses['Program_Unit']
    [<class 'fparser.two.Fortran2003.Main_Program'>, <class 'fparser.two.Fortran2003.Function_Subprogram'>, <class 'fparser.two.Fortran2003.Subroutine_Subprogram'>, <class 'fparser.two.Fortran2003.Module'>, <class 'fparser.two.Fortran2003.Block_Data'>]

Class Generation
++++++++++++++++

Some classes that are specified as strings in the `subclass_names` or
`use_names` variables do not require class implementations. There are 3
categories of these:

1) classes of the form '\*\_Name'
2) classes of the form '\*\_List'
3) classes of the form 'Scalar\_\*'

The reason for this is that such classes can be written in a generic,
boiler-plate way so it is simpler if these are generated rather than
them having to be hand written.

At the end of the Fortran2003.py and Fortran2008.py files there is
code that is executed when the file is imported. This code generates
the required classes described above in the local file.

.. note::

   The way this is implemented needs to be described.

As a practical example, consider rule `R1106`
::

   R1106 end-module-stmt is END [ MODULE [ module-name ] ]

which is implemented in the following way
::

    class End_Module_Stmt(EndStmtBase):  # R1106
        ''' <description> '''
        subclass_names = []
        use_names = ['Module_Name']

        @staticmethod
        def match(string):
            return EndStmtBase.match('MODULE', Module_Name, string)

It can be seen that the `Module_Name` class is specified as a string
in the `use_names` variable. The `Module_Name` class has no
implementation in the Fortran2003.py file, the class is
generated. This code generation is performed when the file is
imported.

.. note::

   At the moment the same code-generation code is replicated in both
   the Fortran2003.py and Fortran2008.py files. It would be better to
   import this code from a separate file if it is possible to do so.

.. _base-classes:

Base classes
++++++++++++

There are a number of base classes implemented to support matching
certain types of pattern in a rule. The two most commonly used are
given below. As mentioned earlier, the class `Base` supports a choice
between classes. The class `BlockBase` supports an initial and final
match with optional subclasses inbetween (useful for matching rules
such as programs, subroutines, if statements etc.).

.. autoclass:: fparser.two.utils.Base
	       :members:

.. autoclass:: fparser.two.utils.BlockBase
	       :members:

.. note::

   The `BlockBase` `match` method is complicated. One way to simplify this
   would be to create a `NamedBlockBase` which subclasses `BlockBase`. This
   would include the code associated with a block having a name.

.. _Fortran2008:

Fortran2008 implementation
++++++++++++++++++++++++++

As Fortran2008 is a superset of Fortran2003, the Fortran2008 classes
are implemented as extensions to the Fortran2003 classes where
possible. For example, the Fortran2003 rule for a program-unit is::
   
    R202 program-unit is main-program
                         or external-subprogram
                         or module
                         or block-data

and for Fortran2008 it is
::
   
    R202 program-unit is main-program
                         or external-subprogram
                         or module
			 or submodule
                         or block-data

Therefore to implement the Fortran2008 version of this class, the
Fortran2003 version needs to be extended with the `subclass_names`
list being extended to include a `Submodule` class as a string (of
course the `Submodule` class also needs to be implemented!)
::

    >>> from fparser.two.Fortran2003 import Program_Unit as Program_Unit_2003

    >>> class Program_Unit(Program_Unit_2003):  # R202
    >>>       ''' <description> '''
    >>>       subclass_names = Program_Unit_2003.subclass_names[:]
    >>>       subclass_names.append("Submodule")


.. _program-class:

Program Class (rule R201)
+++++++++++++++++++++++++

As discussed earlier, Fortran rule `R201` is the 'top level' Fortran
rule. There are no other rules that reference rule `R201`. The rule
looks like this::

    R201 program is program-unit
                    [ program-unit ] ...

which specifies that a Fortran program can consist of one or more program
units. Note, the above rule does not capture the fact that it is valid
to have an arbitrary number of comments before the first program-unit,
inbetween program-units and after the final program-unit.

As the above rule is not a simple choice between different rules a
static `match` method is required for the associated fparser2
`Program` class.

As discussed earlier there are a number of base classes implemented to
support matching certain types of pattern in a rule. The obvious one
to use here would be `BlockBase` as it supports a compulsory first
class, an arbitrary number of optional intermediate classes (provided
as a list) and a final class. Therefore, subclassing `BlockBase` and
setting the first class to `Program_Unit`, the intermediate classes to
`[Program_Unit]`, and the final class to `None` would seem to perform
the required functionality (and this was how it was implemented in
earlier versions of fparser2).

However, there is a problem using `BlockBase`. In the case where there
is no final class (which is the situation here) it is valid for the
first class to match and for an optional class to **fail** to
match. This is not the required behaviour for the `Program` class as, if an
optional `Program_Unit` exists then it must be a valid `Program_Unit`
or the code is invalid. For example, the following code is invalid as
there is a misspelling of `subroutine`::

    program test
    end
    subroutin broken
    end

To implement the required functionality for the `Program` class, the
static `match` method is written manually. A `while` loop is used to
ensure that there is no match if any `Program_Unit` is invalid.

There are also two contraints that must be adhered to by the `Program`
class:

1) Only one program unit may be a main program
2) Any name used by a program-unit (e.g. program fred) must be
   distinct from names used in other program-units.

At the moment neither of these two contraints are enforced in
fparser2. Therefore two xfailing tests `test_one_main1` and
`test_multiple_error1` have been added to the
`tests/fortran2003/test_program_r201.py` file to demonstrate these
limitations.

Further, in Fortran the `program` declaration is actually
optional. For example, the following is a valid (minimal) Fortran
program::

    end

fparser2 does not support the above syntax in its `Program_Unit`
class. Therefore as a workaround, a separate `Program_Unit0` class has
been implemented and added as a final test to the `Program` match
method. This does make use of `BlockBase` to match and therefore
requires the `Program` class to subclass `BlockBase`.

.. note::
   
   It would be much better if `Program_Unit` was coded to support
   optional program declarations and this option should be
   investigated.

The current implementation also has a limitation in that
multiple program-units with one of them not having a program
declaration are not supported. The xfailing test
`test_missing_prog_multi` has been added to the
`tests/fortran2003/test_program_r201.py` file to demonstrate this
limitation.

A final issue is that the line numbers and line information output is
incorrect in certain cases where there is a syntax error in the code
and there are 5 spaces before a statement. The xfailing tests
`test_single2` and `test_single3` have been added to the
`tests/fortran2003/test_program_r201.py` file to demonstrate this
error.

.. _program-unit-class:

Program_Unit Class (rule R202)
++++++++++++++++++++++++++++++

Fortran2003 rule `r202` is specified as
::

    R202 program-unit is main-program
                         or external-subprogram
                         or module
                         or block-data

As the above rule is a simple choice between different rules, the
appropriate matching code is already implemented in one of the base
classes (`Base`) and therefore does not need to be written.  Instead,
the rules on the right hand side can be provided as **strings** in the
`subclass_names` list. The `use_names` list should be empty and the
`tostr` method is not required (as there is no text to output because
this rule is simply used to decide what other rules to use).

.. note::

    it is currently unclear when to use `subclass_names` and when to use
    `use_names`. At the moment the pragmatic suggestion is to follow the
    way it is currently done.

Therefore to implement rule `R202` the following needs to
be specified
::
   
    class Program_Unit(Base):  # R202
        ''' <description> '''
        subclass_names = ['Comment', 'Main_Program', 'External_Subprogram',
                          'Module', 'Block_Data']

In this way fparser2 captures the `R202` rule hierarchy in its
`Program_Unit` class.

.. _exceptions:

Exceptions
++++++++++

There are 6 types of exception raised in fparser2: `NoMatchError`,
`FortranSyntaxError`, `ValueError`, `InternalError`, `AssertionError` and
`NotImplementedError`.

A baseclass `FparserException` is included which `NoMatchError`,
`FortranSyntaxError` and `InternalError` subclass. The reason for this
is to allow external tools to more simply manage fparser if it is used
as a library.

Each of the exceptions are now discussed in turn.

`NoMatchError` can be raised by a class when the text it is given does
not match the pattern for the class. A class can also return an empty
return value to indicate no match. It is currently unclear when it is
appropriate to do one or the other.

`NoMatchError` (or an empty return value) does not necessarily mean that
the text is invalid, just that the text does not match this class. For
example, it may be that some text should match one of a set of
rules. In this case all rules would fail to match except one. It is
only invalid text if none of the possible rules match.

Usually `NoMatchError` is raised by a class with no textual information
(a string provided as an argument to the exception), as textual
information is not required. When textual information is provided this
is ignored.

.. note::

   `NoMatchError` is the place where we can get context-specific
   information about a syntax error. The problem is that there are
   typically many `NoMatchError`s associated with invalid code. The
   reason for this is that every (relevant) rule needs to be matched
   with the associated invalid code. Each of these will return a
   `NoMatchError`. One option would be to always return
   context-specific information from `NoMatchError` and somehow
   aggregate this information until it is known that there is a syntax
   error. At this point a `FortranSyntaxError` is raised and the
   aggregated messages could be used to determine the correct
   message(s) to return. As a simple example, imagine parsing the
   following code: `us mymodule`.  This is probably meant to mean `use
   mymodule`. The associated rule might return a `NoMatchError` saying
   something like `use not found`. However, there might be a missing
   `=` and it could be that an assignment would would also return a
   `NoMatchError` saying something like `invalid assignment`. It is
   unclear which was the programmers intention. In general, it is
   probable that the further into a rule one gets the more likely it
   is a syntax error for that rule, so it may be possible to prune out
   many `NoMatchError`s. There may even be some rule about this
   i.e. if a hierarchy of rules is matched to a certain depth then it
   must be a syntax error associated with this rule. However, in
   general it will not be possible to prune `NoMatchError`s down to one.
   The first step could be to return context information from
   `NoMatchError` for all failures to match and then look at whether
   there is an obvious way to prune these when raising a
   `FortranSyntaxError`.

.. note::

   Need to add an explanation about when `NoMatchError` exceptions are
   used and when a null return is used.

A `FortranSyntaxError` exception should be raised if the parser does
not recognise the syntax. `FortranSyntaxError` takes two
arguments. The first argument is a reader object which allows the line
number and text of the line in question to be output. The second
argument is text which can be used to give details of the error.

Currently the main use of `FortranSyntaxError` is to catch the final
`NoMatchError` exception and re-raise it with line number and the text
of the line to be output. This final `NoMatchError` is caught and
re-raised by overriding the `Base` class `__new__` method in the top
level `Program` class. However, this exception is not able to give any
details of the error as is knows nothing about which rules failed to
match.

`FortranSyntaxError` has started to be used in a few other places
(e.g. curently limited to `BlockBase`), where it is know that there is
a match, but that the match is known to have a syntax error. This
approach leads to good quality feedback to the user on the type of
error and its location and should be used wherever possible. One issue
is that when `FortranSyntaxError` is raised from one of these
additional places the fparser script is not able to use the reader's
fifo buffer to extract position information. This is dealt with by not
outputting anything from the script related to the fifo buffer in this
case. It is possible that if the lines were pushed back into the
buffer then this would work.

.. note::

   more information about the error could be determined by inspecting
   the FortranReader object. In particular, a match can be over a
   number of lines and the first line could be returned as well as the
   last. At the moment the last line and the line number are returned.

A `ValueError` exception is raised if an invalid standard is passed to
the `create` method of the `ParserFactory` class.

An `InternalError` exception is raised when an unexpected condition is
found. Such errors currently specify where there error was, why it
happened and request that the authors are contacted.

.. note::
   
   An additional future idea would be to also wrap the whole code with
   a general exception handler which subsequently raised an
   InternalError. This would catch any additional unforseen errors
   e.g. errors due to the wrong type of data being passed. One
   implementation would be to have this as the the only place an
   InternalError is raised, however, it is considered better to check
   for exceptions where they might happen e.g. a dangling else clause,
   as appropriate contextual information can be given in the
   associated error message.

.. note::

   Information needs to be added about the use of
   `NotImplementedError` and `AssertionError` and/or the code needs to
   be modified. These exceptions come from pre-existing code and it is
   likely that we would want to remove the `AssertionError` from
   fparser. There has also been discussion about using a logger for
   messages, however, there are currently no known situations where it
   makes sense to output messages.

Object Hierarchy
++++++++++++++++

Fortran code is parsed by creating the `Program` object with a
`FortranReader` object as its argument. If the code is parsed
successfully then a hierarchy of objects is returned associated with
the structure of the original code. For example::

    >>> from fparser.common.readfortran import FortranStringReader
    >>> code = "program test\nend"
    >>> reader = FortranStringReader(code)
    >>> ast = parser_f2003(reader)
    >>> ast
    Program(Main_Program(Program_Stmt('PROGRAM', Name('test')), End_Program_Stmt('PROGRAM', None)))

Therefore the above example creates a `Program` object, which contains
a `Main_Program` object. The `Main_Program` object contains a
`Program_Stmt` object followed by an `End_Program_Stmt` object. The
`Program_Stmt` object contains the `PROGRAM` text and a `Name`
object. The `Name` object contains the name of the program
i.e. `test`. The `End_Program_Stmt` object contains the `PROGRAM` text
and a `None` for the name as it is not supplied in the original code.

As one might expect, the object hierarchy adheres to the Fortran rule
hierarchy presented in the associated Fortran specification document
(as each class implements a rule). If one were to manually follow the
rules in the specification document to confirm a code was compliant
and write down the rules visited on a piece of paper in a hierarchical
manner (i.e. also write down which rules triggered subsequent rules)
then there would be a one-to-one correspondance between the rules and
rule hierarchy written on paper and the objects and object hierarchy
returned by fparser2.

Utils
+++++

fparser2 includes a `utils.py` file. This file contains the base
classes discussed in the :ref:`base-classes` section, the
fparser2-specific exceptions discussion in the :ref:`exceptions`
section and a tree-walk utility that can be used to traverse the AST
produced by fparser2 for a valid Fortran program.

.. note::

   the tree-walk utility currently fails if the parent node of the
   tree is provided. The solution is to provide the parent's
   children. This should be fixed at some point.


.. skip
   # Constraints
   # +++++++++++
   # TBD
   # Comment Class
   # +++++++++++++
   # TBD
