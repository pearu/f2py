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
Fortran 2008. It is a little less mature than fparser but is being
actively developed so should be able to replace fparser in the near
future.

Rules
+++++

Each version of the Fortran language is defined as a set of rules in a
specification document. The Fortran2003 rules are specified here
https://j3-fortran.org/doc/year/10/10-007r1.pdf and the Fortran2008
rules are specified here
https://j3-fortran.org/doc/year/10/10-007r1.pdf.

Each rule has a number, for example in the Fortran2003 document we
find the following top level rules `R201` and `R202`: ::

    R201 program is program-unit
                    [ program-unit ] ...

    R202 program-unit is main-program
                         or external-subprogram
                         or module
                         or block-data

It can be seen that the right hand side of these rules consist of more
rules. At some point in the rule hierarchy rules start to be
defined by text. For example, if we look at the specification of a module:
::

    R1104 module is module-stmt
                    [ specification-part ]
                    [ module-subprogram-part ]
                    end-module-stmt
    R1105 module-stmt is MODULE module-name
    R1106 end-module-stmt is END [ MODULE [ module-name ] ]

We can see that rules `R1105` and `R1106` specify the actual code to
write e.g. `MODULE` (where `[]` means that the content is
optional). Here `module-name` is a type of `name` which has a rule
specifying what is valid syntax.

Classes
+++++++

In fparser2 each rule is implemented in a class with the class names
closely following the rule names. For example, `program` is implemented
by the `Program` class and `program-unit` is implemented by the `Program_Unit`
class. In general if you replace '-' with '_' and capitalise each word
in the rule you get the equivalent class.

The Fortran2003 classes are kept in the Fortran2003.py file and the
Fortran2008 classes are kept in the Fortran2008.py file (see
Fortran2008 section for fortran2008-specific implementation details).

The Fortran2003 and Fortran2008 classes can inherit from a set of
pre-existing base classes which implement certain rule patterns in a
generic way. At the moment the base classes are contained in the
Fortran2003.py file. The base classes and rule patterns are discussed
more in the Base classes section.

The primary components of classes i.e. the parts that developers
typically need to be concerned with are:

1) the subclass_names list

2) the use_names list

3) the static match method

4) the tostr method

Example
+++++++

How and whether the above parts of a particular class are used depends
on the type of rule being implemented.

If we go back to rule r202 mentioned in the previous section::

    R202 program-unit is main-program
                         or external-subprogram
                         or module
                         or block-data

we can see that this rule is a simple choice between different
classes. In this situation the rules on the right hand side must be
provided as **strings** in the 'subclass_names' list. The 'use_names'
list should be empty and the 'tostr' method is not required (as there
is no text to output because this rule is simply used to decide what
other rules to use).

Note::
  it is currently unclear when to use subclass_names and when to
  use use_names. At the moment the pragmatic suggestion is to follow the
  way it is currently done.

As the above method is a simple choice between different classes the
appropriate static match method is already implemented in one of the
base classes ('Base') and therefore is not required. So to implement the
above rule we simply need to specify the following::
   
   class Program_Unit(Base):  # R202
       ''' description '''
       subclass_names = ['Comment', 'Main_Program', 'External_Subprogram',
                         'Module', 'Block_Data']


Class Generation
++++++++++++++++

Some classes that are specified as strings are not actually implemented as Classes. There are 3 categories:

1) classes of the form '\*\_Name'

2) classes of the form '\*\_List'

3) classes of the form 'Scalar\_\*'

The reason for this is that such classes can be written in a generic
boiler-plate way so it is easier if these are generated rather than
being hand written.

At the end of the Fortran2003.py and Fortran2008.py files there is
code that is executed when the file is imported. This code generates
the required classes in the file.

So, as a practical example, if we look at rule R1106:
::

    end-module-stmt is END [ MODULE [ <module-name> ] ]

    class End_Module_Stmt(EndStmtBase):  # R1106
        ''' description '''
        """
        subclass_names = []
        use_names = ['Module_Name']

        @staticmethod
        def match(string):
            return EndStmtBase.match('MODULE', Module_Name, string)

we see that the `Module_Name` class is one of the generated
classes. This has no implementation in the Fortran2003.py file, the
class is generated. This code generation is done when the file is
imported. At the moment the same code is replicated in both the
Fortran2003.py and Fortran2008.py files as I did not know how to get
the code to work from outside of the file on which it acts.


Base classes
++++++++++++

As mentioned earlier there are a number of base classes implemented to
support matching certain types of pattern in a rule. The two most
commonly used are given below. As mentioned earlier, Base supports a
choice between classes. BlockBase supports an initial and final match
with optional subclasses inbetween (such as programs, subroutines, if
statements etc.).

.. autoclass:: fparser.two.Fortran2003.Base
	       :members:

.. autoclass:: fparser.two.Fortran2003.BlockBase
	       :members:

Fortran2008 implementation
++++++++++++++++++++++++++

As Fortran2008 is a superset of Fortran2003, the Fortran2008 classes are implemented as extensions to the Fortran2003 classes where possible. For example, the Fortran2003 rule for a program-unit is:
::
   
    R202 program-unit is main-program
                         or external-subprogram
                         or module
                         or block-data

For Fortran2008 it is:
::
   
    R202 program-unit is main-program
                         or external-subprogram
                         or module
			 or submodule
                         or block-data

Therefore to implement the Fortran2008 version of this class we simply
need to modify the `subclass_names` list adding a submodule class:
::

    >>> from fparser.two.Fortran2003 import Program_Unit as Program_Unit_2003

    >>> class Program_Unit(Program_Unit_2003):  # R202
    >>>       ''' description '''
    >>>       subclass_names = Program_Unit_2003.subclass_names[:]
    >>>       subclass_names.append("Sub_Module")

.. skip
   # Constraints
   # +++++++++++
   # TBD
   # Comment Class
   # +++++++++++++
   # TBD
