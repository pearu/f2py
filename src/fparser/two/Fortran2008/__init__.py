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
from fparser.two.Fortran2008.Fortran2008 import (
    Program_Unit,
    Executable_Construct,
    Executable_Construct_C201,
    Action_Stmt,
    Action_Stmt_C201,
    Action_Stmt_C816,
    Action_Stmt_C828,
    Data_Component_Def_Stmt,
    Component_Attr_Spec,
    Type_Declaration_Stmt,
    Codimension_Attr_Spec,
    Coarray_Bracket_Spec,
    Attr_Spec,
    Coarray_Spec,
    Deferred_Coshape_Spec,
    Explicit_Coshape_Spec,
    Coshape_Spec,
    Lower_Cobound,
    Upper_Cobound,
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

from fparser.two.Fortran2008.action_term_do_construct_r824 import (
    Action_Term_Do_Construct,
)
from fparser.two.Fortran2008.block_label_do_construct_r814_1 import (
    Block_Label_Do_Construct,
)
from fparser.two.Fortran2008.block_nonlabel_do_construct_r814_2 import (
    Block_Nonlabel_Do_Construct,
)
from fparser.two.Fortran2008.label_do_stmt_r816 import Label_Do_Stmt
from fparser.two.Fortran2008.nonlabel_do_stmt_r817 import Nonlabel_Do_Stmt


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
