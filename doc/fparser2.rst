
.. _fparser2 :

fparser2
========

fparser2 provides support for parsing Fortran up to and including
Fortran 2003 through the `Fortran2003` module. This is implemented in
the Fortran2003.py `file`__ and contains an entirely separate parser
that includes rules for Fortran 2003 syntax. 

__ https://github.com/stfc/fparser/blob/master/src/fparser/Fortran2003.py

Getting Going
-------------

As with the other parser (:ref:`fparser`), the source code to parse
must be provided via an iterator which is an instance of either
`FortranFileReader` or `FortranStringReader` (see
:ref:`readfortran`). For example:

::
   
    >>> from fparser.api import Fortran2003
    >>> from fparser.readfortran import FortranFileReader
    >>> reader = FortranFileReader("compute_unew_mod.f90")
    >>> program = Fortran2003.Program(reader)
    >>> print program
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
    >>> program
    Program(Module(Module_Stmt('MODULE', Name('compute_unew_mod')), Specification_Part(Use_Stmt(None, Name('kind_params_mod'), '', None), Use_Stmt(None, Name('kernel_mod'), '', None), Use_Stmt(None, Name('argument_mod'), '', None), Use_Stmt(None, Name('grid_mod'), '', None), Use_Stmt(None, Name('field_mod'), '', None), Implicit_Part(Implicit_Stmt('NONE')), Access_Stmt('PRIVATE', None), Access_Stmt('PUBLIC', Name('invoke_compute_unew')), Access_Stmt('PUBLIC', Access_Id_List(',', (Name('compute_unew'), Name('compute_unew_code')))), Derived_Type_Def(Derived_Type_Stmt(Type_Attr_Spec('EXTENDS', Name('kernel_type')), Type_Name('compute_unew'), None), ...

Data Model
----------

The module provides the classes; `Main_Program`,
`Subroutine_Subprogram`, `Function_Subprogram`, `Program_Stmt`,
`Function_Stmt`, `Subroutine_Stmt`, `Block_Do_Construct`,
`Block_Label_Do_Construct`, `Block_Nonlabel_Do_Construct`,
`Execution_Part`, `Name` and `Constant`, amongst others.  Nodes in the
tree representing the parsed code are instances of either `BlockBase`
or `SequenceBase`. Child nodes are then stored in the `.content`
attribute of `BlockBase` objects or the `.items` attribute of
`SequenceBase` objects. Both of these attributes are Tuple instances.
