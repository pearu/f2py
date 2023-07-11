# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------

"""
Fortran 2008 module. Contains classes which extend the Fortran
2003 standard to implement the Fortran 2008 standard.

"""
import inspect
import sys

from fparser.two.Fortran2003 import Base, SequenceBase
from fparser.two.Fortran2008.program_unit_r202 import Program_Unit
from fparser.two.Fortran2008.executable_construct_r213 import Executable_Construct
from fparser.two.Fortran2008.executable_construct_c201 import Executable_Construct_C201
from fparser.two.Fortran2008.action_stmt_r214 import Action_Stmt
from fparser.two.Fortran2008.action_stmt_c201 import Action_Stmt_C201
from fparser.two.Fortran2008.action_stmt_c816 import Action_Stmt_C816
from fparser.two.Fortran2008.action_stmt_c828 import Action_Stmt_C828
from fparser.two.Fortran2008.data_component_def_stmt_r436 import Data_Component_Def_Stmt
from fparser.two.Fortran2008.component_attr_spec_r437 import Component_Attr_Spec
from fparser.two.Fortran2008.type_declaration_stmt_r501 import Type_Declaration_Stmt
from fparser.two.Fortran2008.codimension_attr_spec_r502d import Codimension_Attr_Spec
from fparser.two.Fortran2008.coarray_bracket_spec_r502d0 import Coarray_Bracket_Spec
from fparser.two.Fortran2008.attr_spec_r502 import Attr_Spec
from fparser.two.Fortran2008.coarray_spec_r509 import Coarray_Spec
from fparser.two.Fortran2008.deferred_coshape_spec_r510 import Deferred_Coshape_Spec
from fparser.two.Fortran2008.explicit_coshape_spec_r511 import Explicit_Coshape_Spec
from fparser.two.Fortran2008.coshape_spec_r511a import Coshape_Spec
from fparser.two.Fortran2008.lower_cobound_r512 import Lower_Cobound
from fparser.two.Fortran2008.upper_cobound_r513 import Upper_Cobound
from fparser.two.Fortran2008.Fortran2008 import (
    Do_Term_Action_Stmt,
    Alloc_Opt,
    Allocate_Stmt,
    Loop_Control,
    If_Stmt,
    Error_Stop_Stmt,
    Specification_Part_C1112,
    Implicit_Part_C1112,
    Implicit_Part_Stmt_C1112,
    Declaration_Construct_C1112,
    Submodule,
    Submodule_Stmt,
    End_Submodule_Stmt,
    Parent_Identifier,
    Open_Stmt,
    Connect_Spec,
    Block_Construct,
    Block_Stmt,
    End_Block_Stmt,
    Critical_Construct,
    Critical_Stmt,
    End_Critical_Stmt,
    Procedure_Stmt,
)


# pylint: disable=eval-used
# pylint: disable=exec-used

#
# GENERATE Scalar_, _List, _Name CLASSES
#
ClassType = type(Base)
_names = dir()
for clsname in _names:
    NEW_CLS = eval(clsname)
    if not (
        isinstance(NEW_CLS, ClassType)
        and issubclass(NEW_CLS, Base)
        and not NEW_CLS.__name__.endswith("Base")
    ):
        continue

    names = getattr(NEW_CLS, "subclass_names", []) + getattr(NEW_CLS, "use_names", [])
    for n in names:
        if n in _names:
            continue
        if n.endswith("_List"):
            _names.append(n)
            n = n[:-5]
            # Generate 'list' class
            exec(
                f"""\
class {n}_List(SequenceBase):
    subclass_names = [\'{n}\']
    use_names = []
    @staticmethod
    def match(string): return SequenceBase.match(r\',\', {n}, string)
"""
            )
        elif n.endswith("_Name"):
            _names.append(n)
            n = n[:-5]
            exec(
                f"""\
class {n}_Name(Base):
    subclass_names = [\'Name\']
"""
            )
        elif n.startswith("Scalar_"):
            _names.append(n)
            n = n[7:]
            exec(
                f"""\
class Scalar_{n}(Base):
    subclass_names = [\'{n}\']
"""
            )
# Make sure NEW_CLS does not reference a class so is not accidentally
# picked up in __all__.
NEW_CLS = None


# Determine the generated classes in this module and list these in
# __all__ to support automatic documentation generation with AutoDoc.

classes = inspect.getmembers(
    sys.modules[__name__],
    lambda member: inspect.isclass(member) and member.__module__ == __name__,
)
__all__ = [name[0] for name in classes if name[0]]
