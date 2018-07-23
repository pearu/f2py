# Modified work Copyright (c) 2017-2018 Science and Technology
# Facilities Council
# Original work Copyright (c) 1999-2008 Pearu Peterson

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

from fparser.two.Fortran2003 import *
from fparser.api import get_reader

import pytest


def assertRaises(exc, cls, s):
    try:
        cls(s)
        raise AssertionError('Expected %s but got nothing' % exc)
    except exc:
        pass


#
# SECTION 2
#


def test_Program():  # R201
    ''' Tests for parsing top-level program unit '''
    cls = Program
    reader = get_reader('''\
      subroutine foo
      end subroutine Foo
      subroutine bar
      end
      ''')
    obj = cls(reader)
    assert isinstance(obj, cls), repr(obj)
    print(str(obj))
    assert "SUBROUTINE foo\nEND SUBROUTINE Foo\nSUBROUTINE bar\n" \
        "END SUBROUTINE bar" in str(obj)

    reader = get_reader('''\
      subroutine foo (*)
      end subroutine foo
      ''')
    obj = cls(reader)
    assert isinstance(obj, cls), repr(obj)
    assert 'SUBROUTINE foo(*)\nEND SUBROUTINE foo' in str(obj)


def test_Specification_Part():  # R204
    ''' Tests for parsing specification-part '''
    reader = get_reader('''\
    integer a''')
    cls = Specification_Part
    obj = cls(reader)
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTEGER :: a'
    assert (repr(obj) == "Specification_Part(Type_Declaration_Stmt("
            "Intrinsic_Type_Spec('INTEGER', None), None, "
            "Entity_Decl(Name('a'), None, None, None)))")

    obj = cls(get_reader('''\
type a
end type a
type b
end type b
'''))
    assert isinstance(obj, cls), repr(obj)
    assert 'TYPE :: a\nEND TYPE a\nTYPE :: b\nEND TYPE b' in str(obj)

#
# SECTION  3
#


def test_Name():  # R304

    obj = Name('a')
    assert isinstance(obj, Name), repr(obj)
    obj = Name('a2')
    assert isinstance(obj, Name), repr(obj)
    obj = Designator('a')
    assert isinstance(obj, Name), repr(obj)
    obj = Constant('a')
    assert isinstance(obj, Name), repr(obj)
    obj = Expr('a')
    assert isinstance(obj, Name), repr(obj)


def test_Literal_Constant():  # R305

    cls = Constant
    obj = cls('.false.')
    assert isinstance(obj, Logical_Literal_Constant), repr(obj)
    assert str(obj) == '.FALSE.'


def test_Literal_Constant():  # R306

    cls = Literal_Constant
    obj = cls('.false.')
    assert isinstance(obj, Logical_Literal_Constant), repr(obj)
    assert str(obj) == '.FALSE.'

#
# SECTION 4
#


def test_Type_Param_Value():  # 402

    cls = Type_Param_Value
    obj = cls('*')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '*'
    assert repr(obj) == "Type_Param_Value('*')"

    obj = cls(':')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == ':'

    obj = cls('1+2')
    assert isinstance(obj, Level_2_Expr), repr(obj)
    assert str(obj) == '1 + 2'


def test_Intrinsic_Type_Spec():  # R403

    cls = Intrinsic_Type_Spec
    obj = cls('INTEGER')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTEGER'
    assert repr(obj), "Intrinsic_Type_Spec('INTEGER', None)"

    obj = cls('Integer*2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTEGER*2'

    obj = cls('real*2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'REAL*2'

    obj = cls('logical*2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'LOGICAL*2'

    obj = cls('complex*2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'COMPLEX*2'

    obj = cls('character*2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'CHARACTER*2'

    obj = cls('double complex')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DOUBLE COMPLEX'

    obj = cls('double  precision')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DOUBLE PRECISION'


def test_Kind_Selector():  # R404

    cls = Kind_Selector
    obj = cls('(1)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(KIND = 1)'
    assert (repr(obj) ==
            "Kind_Selector('(', Int_Literal_Constant('1', None), ')')")

    obj = cls('(kind=1+2)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(KIND = 1 + 2)'

    obj = cls('* 1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '*1'


def test_Signed_Int_Literal_Constant():  # R405

    cls = Signed_Int_Literal_Constant
    obj = cls('1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '1'
    assert repr(obj) == "%s('1', None)" % (cls.__name__)

    obj = cls('+ 21_2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '+21_2'
    assert repr(obj) == "%s('+21', '2')" % (cls.__name__)

    obj = cls('-21_SHORT')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '-21_SHORT'

    obj = cls('21_short')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '21_short'

    obj = cls('+1976354279568241_8')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '+1976354279568241_8'


def test_Int_Literal_Constant():  # R406

    cls = Int_Literal_Constant
    obj = cls('1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '1'
    assert repr(obj) == "%s('1', None)" % (cls.__name__)

    obj = cls('21_2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '21_2'
    assert repr(obj) == "%s('21', '2')" % (cls.__name__)

    obj = cls('21_SHORT')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '21_SHORT'

    obj = cls('21_short')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '21_short'

    obj = cls('1976354279568241_8')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '1976354279568241_8'


def test_Binary_Constant():  # R412

    cls = Boz_Literal_Constant
    bcls = Binary_Constant
    obj = cls('B"01"')
    assert isinstance(obj, bcls), repr(obj)
    assert str(obj) == 'B"01"'
    assert repr(obj) == "%s('B\"01\"')" % (bcls.__name__)


def test_Octal_Constant():  # R413

    cls = Boz_Literal_Constant
    ocls = Octal_Constant
    obj = cls('O"017"')
    assert isinstance(obj, ocls), repr(obj)
    assert str(obj) == 'O"017"'
    assert repr(obj) == "%s('O\"017\"')" % (ocls.__name__)


def test_Hex_Constant():  # R414

    cls = Boz_Literal_Constant
    zcls = Hex_Constant
    obj = cls('Z"01A"')
    assert isinstance(obj, zcls), repr(obj)
    assert str(obj) == 'Z"01A"'
    assert repr(obj) == "%s('Z\"01A\"')" % (zcls.__name__)


def test_Signed_Real_Literal_Constant():  # R416

    cls = Signed_Real_Literal_Constant
    obj = cls('12.78')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '12.78'
    assert repr(obj) == "%s('12.78', None)" % (cls.__name__)

    obj = cls('+12.78_8')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '+12.78_8'
    assert repr(obj) == "%s('+12.78', '8')" % (cls.__name__)

    obj = cls('- 12.')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '-12.'

    obj = cls('1.6E3')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '1.6E3'

    obj = cls('+1.6E3_8')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '+1.6E3_8'

    obj = cls('1.6D3')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '1.6D3'

    obj = cls('-1.6E-3')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '-1.6E-3'
    obj = cls('1.6E+3')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '1.6E+3'

    obj = cls('3E4')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '3E4'

    obj = cls('.123')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '.123'

    obj = cls('+1.6E-3')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '+1.6E-3'

    obj = cls('10.9E7_QUAD')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '10.9E7_QUAD'

    obj = cls('-10.9e-17_quad')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '-10.9E-17_quad'


def test_Real_Literal_Constant():  # R417

    cls = Real_Literal_Constant
    obj = cls('12.78')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '12.78'
    assert repr(obj) == "%s('12.78', None)" % (cls.__name__)

    obj = cls('12.78_8')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '12.78_8'
    assert repr(obj) == "%s('12.78', '8')" % (cls.__name__)

    obj = cls('12.')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '12.'

    obj = cls('1.6E3')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '1.6E3'

    obj = cls('1.6E3_8')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '1.6E3_8'

    obj = cls('1.6D3')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '1.6D3'

    obj = cls('1.6E-3')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '1.6E-3'
    obj = cls('1.6E+3')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '1.6E+3'

    obj = cls('3E4')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '3E4'

    obj = cls('.123')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '.123'

    obj = cls('1.6E-3')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '1.6E-3'

    obj = cls('10.9E7_QUAD')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '10.9E7_QUAD'

    obj = cls('10.9e-17_quad')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '10.9E-17_quad'

    obj = cls('0.0D+0')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '0.0D+0'


def test_Char_Selector():  # R424

    cls = Char_Selector
    obj = cls('(len=2, kind=8)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(LEN = 2, KIND = 8)'
    assert (repr(obj) ==
            "Char_Selector(Int_Literal_Constant('2', None), "
            "Int_Literal_Constant('8', None))")

    obj = cls('(2, kind=8)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(LEN = 2, KIND = 8)'

    obj = cls('(2, 8)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(LEN = 2, KIND = 8)'

    obj = cls('(kind=8)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(KIND = 8)'

    obj = cls('(kind=8,len=2)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(LEN = 2, KIND = 8)'


def test_Complex_Literal_Constant():  # R421

    cls = Complex_Literal_Constant
    obj = cls('(1.0, -1.0)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(1.0, -1.0)'
    assert (repr(obj) ==
            "Complex_Literal_Constant(Signed_Real_Literal_Constant("
            "'1.0', None), Signed_Real_Literal_Constant('-1.0', None))")

    obj = cls('(3,3.1E6)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(3, 3.1E6)'

    obj = cls('(4.0_4, 3.6E7_8)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(4.0_4, 3.6E7_8)'

    obj = cls('( 0., PI)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(0., PI)'


def test_Type_Name():  # C424

    cls = Type_Name
    obj = cls('a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a'
    assert repr(obj) == "Type_Name('a')"

    assertRaises(NoMatchError, cls, 'integer')
    assertRaises(NoMatchError, cls, 'doubleprecision')


def test_Length_Selector():  # R425

    cls = Length_Selector
    obj = cls('( len = *)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(LEN = *)'
    assert (repr(obj) ==
            "Length_Selector('(', Type_Param_Value('*'), ')')")

    obj = cls('*2,')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '*2'


def test_Char_Length():  # R426

    cls = Char_Length
    obj = cls('(1)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(1)'
    assert (repr(obj) ==
            "Char_Length('(', Int_Literal_Constant('1', None), ')')")

    obj = cls('1')
    assert isinstance(obj, Int_Literal_Constant), repr(obj)
    assert str(obj) == '1'

    obj = cls('(*)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(*)'

    obj = cls('(:)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(:)'


def test_Char_Literal_Constant():  # R427

    cls = Char_Literal_Constant
    obj = cls('NIH_"DO"')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'NIH_"DO"'
    assert repr(obj) == 'Char_Literal_Constant(\'"DO"\', \'NIH\')'

    obj = cls("'DO'")
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == "'DO'"
    assert repr(obj) == 'Char_Literal_Constant("\'DO\'", None)'

    obj = cls("'DON''T'")
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == "'DON''T'"

    obj = cls('"DON\'T"')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '"DON\'T"'

    obj = cls('""')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '""'

    obj = cls("''")
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == "''"

    obj = cls('"hey ha(ada)\t"')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '"hey ha(ada)\t"'


def test_Logical_Literal_Constant():  # R428

    cls = Logical_Literal_Constant
    obj = cls('.TRUE.')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '.TRUE.'
    assert repr(obj) == "%s('.TRUE.', None)" % (cls.__name__)

    obj = cls('.True.')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '.TRUE.'

    obj = cls('.FALSE.')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '.FALSE.'

    obj = cls('.false.')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '.FALSE.'

    obj = cls('.TRUE._HA')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '.TRUE._HA'


def test_Derived_Type_Stmt():  # R430

    cls = Derived_Type_Stmt
    obj = cls('type a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'TYPE :: a'
    assert repr(obj) == "Derived_Type_Stmt(None, Type_Name('a'), None)"

    obj = cls('type ::a(b,c)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'TYPE :: a(b, c)'

    obj = cls('type, private, abstract::a(b,c)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'TYPE, PRIVATE, ABSTRACT :: a(b, c)'


def test_Type_Name():  # C423

    cls = Type_Name
    obj = cls('a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a'
    assert repr(obj) == "Type_Name('a')"


def test_Type_Attr_Spec():  # R431

    cls = Type_Attr_Spec
    obj = cls('abstract')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'ABSTRACT'
    assert repr(obj) == "Type_Attr_Spec('ABSTRACT', None)"

    obj = cls('bind (c )')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'BIND(C)'

    obj = cls('extends(a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'EXTENDS(a)'

    obj = cls('private')
    assert isinstance(obj, Access_Spec), repr(obj)
    assert str(obj) == 'PRIVATE'


def test_End_Type_Stmt():  # R433

    cls = End_Type_Stmt
    obj = cls('end type')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'END TYPE'
    assert repr(obj) == "End_Type_Stmt('TYPE', None)"

    obj = cls('end type  a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'END TYPE a'


def test_Sequence_Stmt():  # R434

    cls = Sequence_Stmt
    obj = cls('sequence')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SEQUENCE'
    assert repr(obj) == "Sequence_Stmt('SEQUENCE')"


def test_Type_Param_Def_Stmt():  # R435

    cls = Type_Param_Def_Stmt
    obj = cls('integer ,kind :: a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTEGER, KIND :: a'
    assert (repr(obj) ==
            "Type_Param_Def_Stmt(None, Type_Param_Attr_Spec('KIND'), "
            "Name('a'))")

    obj = cls('integer*2 ,len :: a=3, b=2+c')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTEGER*2, LEN :: a = 3, b = 2 + c'


def test_Type_Param_Decl():  # R436

    cls = Type_Param_Decl
    obj = cls('a=2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a = 2'
    assert (repr(obj) ==
            "Type_Param_Decl(Name('a'), '=', Int_Literal_Constant('2', None))")

    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'


def test_Type_Param_Attr_Spec():  # R437

    cls = Type_Param_Attr_Spec
    obj = cls('kind')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'KIND'
    assert repr(obj) == "Type_Param_Attr_Spec('KIND')"

    obj = cls('len')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'LEN'


def test_Component_Attr_Spec():  # R441

    cls = Component_Attr_Spec
    obj = cls('pointer')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'POINTER'
    assert repr(obj) == "Component_Attr_Spec('POINTER')"

    obj = cls('allocatable')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'ALLOCATABLE'

    obj = cls('dimension(a)')
    assert isinstance(obj, Dimension_Component_Attr_Spec), repr(obj)
    assert str(obj) == 'DIMENSION(a)'

    obj = cls('private')
    assert isinstance(obj, Access_Spec), repr(obj)
    assert str(obj) == 'PRIVATE'


def test_Component_Decl():  # R442

    cls = Component_Decl
    obj = cls('a(1)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(1)'
    assert (repr(obj) ==
            "Component_Decl(Name('a'), Explicit_Shape_Spec(None, "
            "Int_Literal_Constant('1', None)), None, None)")

    obj = cls('a(1)*(3)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(1)*(3)'

    obj = cls('a(1)*(3) = 2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(1)*(3) = 2'

    obj = cls('a(1) => NULL')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(1) => NULL'


def test_Proc_Component_Def_Stmt():  # R445
    cls = Proc_Component_Def_Stmt
    obj = cls('procedure(), pointer :: a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PROCEDURE(), POINTER :: a'

    obj = cls('procedure(real*8), pointer, pass(n) :: a, b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PROCEDURE(REAL*8), POINTER, PASS(n) :: a, b'


def test_Private_Components_Stmt():  # pylint: disable=invalid-name
    ''' Tests that declaration of PRIVATE components in a type definition
    is parsed correctly (R447) '''
    cls = Private_Components_Stmt
    obj = cls('private')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PRIVATE'
    assert repr(obj) == "Private_Components_Stmt('PRIVATE')"

    # Statement not 'private'
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('public')
    assert "Private_Components_Stmt: 'public'" in str(excinfo)


def test_Type_Bound_Procedure_Part():
    ''' Tests for type-bound procedure, R448 '''
    cls = Type_Bound_Procedure_Part
    obj = cls(get_reader('''\
contains
procedure, pass :: length => point_length'''))
    assert isinstance(obj, cls), repr(obj)
    assert 'CONTAINS\nPROCEDURE, PASS :: length => point_length' in str(obj)


def test_Proc_Binding_Stmt():  # R450
    cls = Proc_Binding_Stmt
    obj = cls('procedure, pass :: length => point_length')
    assert isinstance(obj, Specific_Binding), repr(obj)
    assert str(obj) == 'PROCEDURE, PASS :: length => point_length'


def test_Specific_Binding():  # R451
    cls = Specific_Binding
    obj = cls('procedure, pass :: length => point_length')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PROCEDURE, PASS :: length => point_length'


def test_Generic_Binding():  # R452
    cls = Generic_Binding
    obj = cls('generic :: a => b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'GENERIC :: a => b'

    obj = cls('generic, private :: read(formatted) => b,c')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'GENERIC, PRIVATE :: READ(FORMATTED) => b, c'


def test_Final_Binding():  # R454

    cls = Final_Binding
    obj = cls('final a, b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'FINAL :: a, b'
    assert (repr(obj) ==
            "Final_Binding('FINAL', Final_Subroutine_Name_List(',', "
            "(Name('a'), Name('b'))))")

    obj = cls('final::a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'FINAL :: a'


def test_Derived_Type_Spec():  # R455

    cls = Derived_Type_Spec
    obj = cls('a(b)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(b)'
    assert repr(obj) == "Derived_Type_Spec(Type_Name('a'), Name('b'))"

    obj = cls('a(b,c,g=1)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(b, c, g = 1)'

    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'

    obj = cls('a()')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a()'


def test_Type_Param_Spec():  # R456

    cls = Type_Param_Spec
    obj = cls('a=1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a = 1'
    assert (repr(obj) ==
            "Type_Param_Spec(Name('a'), Int_Literal_Constant('1', None))")

    obj = cls('k=a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'k = a'

    obj = cls('k=:')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'k = :'


def test_Type_Param_Spec_List():  # R456-list

    cls = Type_Param_Spec_List

    obj = cls('a,b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a, b'
    assert (repr(obj) ==
            "Type_Param_Spec_List(',', (Name('a'), Name('b')))")

    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)

    obj = cls('k=a,c,g=1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'k = a, c, g = 1'


def test_Structure_Constructor_2():  # R457.b

    cls = Structure_Constructor_2
    obj = cls('k=a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'k = a'
    assert repr(obj) == "Structure_Constructor_2(Name('k'), Name('a'))"

    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'


def test_Structure_Constructor():  # R457

    cls = Structure_Constructor
    obj = cls('t()')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 't()'
    assert repr(obj) == "Structure_Constructor(Type_Name('t'), None)"

    obj = cls('t(s=1, a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 't(s = 1, a)'

    obj = cls('a=k')
    assert isinstance(obj, Structure_Constructor_2), repr(obj)
    assert str(obj) == 'a = k'
    assert repr(obj) == "Structure_Constructor_2(Name('a'), Name('k'))"

    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'


def test_Component_Spec():  # R458

    cls = Component_Spec
    obj = cls('k=a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'k = a'
    assert repr(obj) == "Component_Spec(Name('k'), Name('a'))"

    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'

    obj = cls('a % b')
    assert isinstance(obj, Proc_Component_Ref), repr(obj)
    assert str(obj) == 'a % b'

    obj = cls('s =a % b')
    assert isinstance(obj, Component_Spec), repr(obj)
    assert str(obj) == 's = a % b'


def test_Component_Spec_List():  # R458-list

    cls = Component_Spec_List
    obj = cls('k=a, b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'k = a, b'
    assert (repr(obj) ==
            "Component_Spec_List(',', (Component_Spec(Name('k'), "
            "Name('a')), Name('b')))")

    obj = cls('k=a, c')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'k = a, c'


def test_Enum_Def():  # R460
    cls = Enum_Def
    obj = cls(get_reader('''\
enum, bind(c)
enumerator :: red = 4, blue = 9
enumerator yellow
end enum
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            "ENUM, BIND(C)\n  ENUMERATOR :: red = 4, blue = 9\n"
            "  ENUMERATOR :: yellow\nEND ENUM")


def test_Enum_Def_Stmt():  # R461
    cls = Enum_Def_Stmt
    obj = cls('enum, bind(c)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'ENUM, BIND(C)'


def test_Array_Constructor():  # R465

    cls = Array_Constructor
    obj = cls('(/a/)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(/a/)'
    assert repr(obj) == "Array_Constructor('(/', Name('a'), '/)')"

    obj = cls('[a]')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '[a]'
    assert repr(obj) == "Array_Constructor('[', Name('a'), ']')"

    obj = cls('[integer::a]')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '[INTEGER :: a]'

    obj = cls('[integer::a,b]')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '[INTEGER :: a, b]'


def test_Ac_Spec():  # R466

    cls = Ac_Spec
    obj = cls('integer ::')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTEGER ::'
    assert (repr(obj) ==
            "Ac_Spec(Intrinsic_Type_Spec('INTEGER', None), None)")

    obj = cls('integer :: a,b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTEGER :: a, b'

    obj = cls('a,b')
    assert isinstance(obj, Ac_Value_List), repr(obj)
    assert str(obj) == 'a, b'

    obj = cls('integer :: a, (a, b, n = 1, 5)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTEGER :: a, (a, b, n = 1, 5)'


def test_Ac_Value_List():  # R469-list

    cls = Ac_Value_List
    obj = cls('a, b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a, b'
    assert repr(obj) == "Ac_Value_List(',', (Name('a'), Name('b')))"

    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'


def test_Ac_Implied_Do():  # R470

    cls = Ac_Implied_Do
    obj = cls('( a, b, n = 1, 5 )')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(a, b, n = 1, 5)'
    assert (repr(obj) ==
            "Ac_Implied_Do(Ac_Value_List(',', (Name('a'), Name('b'))), "
            "Ac_Implied_Do_Control(Name('n'), [Int_Literal_Constant('1', "
            "None), Int_Literal_Constant('5', None)]))")


def test_Ac_Implied_Do_Control():  # R471

    cls = Ac_Implied_Do_Control
    obj = cls('n = 3, 5')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'n = 3, 5'
    assert (repr(obj) ==
            "Ac_Implied_Do_Control(Name('n'), [Int_Literal_Constant('3', "
            "None), Int_Literal_Constant('5', None)])")

    obj = cls('n = 3+1, 5, 1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'n = 3 + 1, 5, 1'

#
# SECTION 5
#


def test_Type_Declaration_Stmt():  # R501

    cls = Type_Declaration_Stmt
    obj = cls('integer a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTEGER :: a'
    assert (repr(obj) ==
            "Type_Declaration_Stmt(Intrinsic_Type_Spec('INTEGER', None), "
            "None, Entity_Decl(Name('a'), None, None, None))")

    obj = cls('integer ,dimension(2):: a*3')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTEGER, DIMENSION(2) :: a*3'

    obj = cls('real a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'REAL :: a'
    assert (repr(obj) ==
            "Type_Declaration_Stmt(Intrinsic_Type_Spec('REAL', None), "
            "None, Entity_Decl(Name('a'), None, None, None))")

    obj = cls('REAL A( LDA, * ), B( LDB, * )')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'REAL :: A(LDA, *), B(LDB, *)'

    obj = cls('DOUBLE PRECISION   ALPHA, BETA')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DOUBLE PRECISION :: ALPHA, BETA'

    obj = cls('logical,parameter:: T=.true.')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'LOGICAL, PARAMETER :: T = .TRUE.'

    obj = cls('character(n),private:: x(n)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'CHARACTER(LEN = n), PRIVATE :: x(n)'

    obj = cls('character(lenmax),private:: x(n)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'CHARACTER(LEN = lenmax), PRIVATE :: x(n)'


def test_Declaration_Type_Spec():  # R502

    cls = Declaration_Type_Spec
    obj = cls('Integer*2')
    assert isinstance(obj, Intrinsic_Type_Spec), repr(obj)
    assert str(obj) == 'INTEGER*2'

    obj = cls('type(foo)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'TYPE(foo)'
    assert repr(obj) == "Declaration_Type_Spec('TYPE', Type_Name('foo'))"


def test_Attr_Spec():  # R503

    cls = Attr_Spec
    obj = cls('allocatable')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'ALLOCATABLE'

    obj = cls('dimension(a)')
    assert isinstance(obj, Dimension_Attr_Spec), repr(obj)
    assert str(obj) == 'DIMENSION(a)'


def test_Dimension_Attr_Spec():  # R503.d

    cls = Dimension_Attr_Spec
    obj = cls('dimension(a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DIMENSION(a)'
    assert (repr(obj) ==
            "Dimension_Attr_Spec('DIMENSION', Explicit_Shape_Spec(None, "
            "Name('a')))")


def test_Intent_Attr_Spec():  # R503.f

    cls = Intent_Attr_Spec
    obj = cls('intent(in)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTENT(IN)'
    assert repr(obj) == "Intent_Attr_Spec('INTENT', Intent_Spec('IN'))"


def test_Entity_Decl():  # 504

    cls = Entity_Decl
    obj = cls('a(1)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(1)'
    assert (repr(obj) ==
            "Entity_Decl(Name('a'), Explicit_Shape_Spec(None, "
            "Int_Literal_Constant('1', None)), None, None)")

    obj = cls('a(1)*(3)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(1)*(3)'

    obj = cls('a(1)*(3) = 2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(1)*(3) = 2'

    obj = cls('a = 2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a = 2'

    obj = cls('a=2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a = 2'

    obj = cls('a = "abc "')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a = "abc "'

    obj = cls('a = .true.')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a = .TRUE.'


def test_Target_Entity_Decl():
    cls = Target_Entity_Decl
    obj = cls('a(1)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(1)'
    assert (repr(obj) ==
            "Target_Entity_Decl(Name('a'), Explicit_Shape_Spec(None, "
            "Int_Literal_Constant('1', None)), None, None)")


def test_Access_Spec():  # R508

    cls = Access_Spec
    obj = cls('private')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PRIVATE'
    assert repr(obj) == "Access_Spec('PRIVATE')"

    obj = cls('public')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PUBLIC'


def test_Language_Binding_Spec():  # R509

    cls = Language_Binding_Spec
    obj = cls('bind(c)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'BIND(C)'
    assert repr(obj) == 'Language_Binding_Spec(None)'

    obj = cls('bind(c, name="hey")')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'BIND(C, NAME = "hey")'


def test_Explicit_Shape_Spec():  # R511

    cls = Explicit_Shape_Spec
    obj = cls('a:b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a : b'
    assert repr(obj) == "Explicit_Shape_Spec(Name('a'), Name('b'))"

    obj = cls('a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a'


def test_Upper_Bound():  # R513

    cls = Upper_Bound
    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'

    assertRaises(NoMatchError, cls, '*')


def test_Assumed_Shape_Spec():  # R514

    cls = Assumed_Shape_Spec
    obj = cls(':')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == ':'
    assert repr(obj) == 'Assumed_Shape_Spec(None, None)'

    obj = cls('a :')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a :'


def test_Deferred_Shape_Spec():  # R515

    cls = Deferred_Shape_Spec
    obj = cls(':')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == ':'
    assert repr(obj) == 'Deferred_Shape_Spec(None, None)'


def test_Assumed_Size_Spec():  # R516

    cls = Assumed_Size_Spec
    obj = cls('*')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '*'
    assert repr(obj) == 'Assumed_Size_Spec(None, None)'

    obj = cls('1:*')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '1 : *'

    obj = cls('a,1:*')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a, 1 : *'

    obj = cls('a:b,1:*')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a : b, 1 : *'


def test_Access_Stmt():  # R518

    cls = Access_Stmt
    obj = cls('private')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PRIVATE'
    assert repr(obj) == "Access_Stmt('PRIVATE', None)"

    obj = cls('public a,b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PUBLIC :: a, b'

    obj = cls('public ::a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PUBLIC :: a'


def test_Data_Stmt():  # R524
    cls = Data_Stmt
    obj = cls('DATA YOURNAME % AGE, YOURNAME % NAME / 35, "FRED BROWN" /')
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'DATA YOURNAME % AGE, YOURNAME % NAME / 35, "FRED BROWN" /')

    obj = cls('DATA NAME / "JOHN DOE" / MILES / 10 * 0 /')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DATA NAME / "JOHN DOE" /, MILES / 10 * 0 /'

    obj = cls('DATA MYNAME / PERSON (21, \'JOHN SMITH\') /')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DATA MYNAME / PERSON(21, \'JOHN SMITH\') /'


def test_Data_Stmt_Set():  # R525
    cls = Data_Stmt_Set
    obj = cls('MILES / 10 * "2/3" /')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'MILES / 10 * "2/3" /'


def test_Data_Implied_Do():  # R527
    cls = Data_Implied_Do
    obj = cls('((SKEW (K, J), J = 1, K), K = 1, 100)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '((SKEW(K, J), J = 1, K), K = 1, 100)'

# R531-R534 are trivial


def test_Dimension_Stmt():  # R535

    cls = Dimension_Stmt
    obj = cls('dimension :: a(5)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DIMENSION :: a(5)'
    assert (repr(obj) ==
            "Dimension_Stmt([(Name('a'), Explicit_Shape_Spec(None, "
            "Int_Literal_Constant('5', None)))])")

    obj = cls('dimension a(n,m), b(:), c(2:n), d(*), e(n, 2:*)')
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'DIMENSION :: a(n, m), b(:), c(2 : n), d(*), e(n, 2 : *)')


def test_Intent_Stmt():  # R536

    cls = Intent_Stmt
    obj = cls('intent(in) :: a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTENT(IN) :: a'
    assert repr(obj) == "Intent_Stmt(Intent_Spec('IN'), Name('a'))"

    obj = cls('intent(out) a, b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTENT(OUT) :: a, b'
    assert (repr(obj) ==
            "Intent_Stmt(Intent_Spec('OUT'), Dummy_Arg_Name_List(',', "
            "(Name('a'), Name('b'))))")


def test_Optional_Stmt():  # R537

    cls = Optional_Stmt
    obj = cls('optional :: a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'OPTIONAL :: a'
    assert repr(obj) == "Optional_Stmt('OPTIONAL', Name('a'))"

    obj = cls('optional :: a, b, c')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'OPTIONAL :: a, b, c'
    assert (repr(obj) ==
            "Optional_Stmt('OPTIONAL', Dummy_Arg_Name_List(',', (Name('a'), "
            "Name('b'), Name('c'))))")


def test_Parameter_Stmt():  # R538

    cls = Parameter_Stmt
    obj = cls('parameter(a=1)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PARAMETER(a = 1)'
    assert (repr(obj) ==
            "Parameter_Stmt('PARAMETER', Named_Constant_Def(Name('a'), "
            "Int_Literal_Constant('1', None)))")

    obj = cls('parameter(a=1, b=a+2)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PARAMETER(a = 1, b = a + 2)'

    obj = cls('PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PARAMETER(ONE = 1.0D+0, ZERO = 0.0D+0)'


def test_Named_Constant_Def():  # R539

    cls = Named_Constant_Def
    obj = cls('a=1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a = 1'
    assert (repr(obj) ==
            "Named_Constant_Def(Name('a'), Int_Literal_Constant('1', None))")


def test_Pointer_Stmt():  # R540

    cls = Pointer_Stmt
    obj = cls('pointer a(:), b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'POINTER :: a(:), b'
    assert (repr(obj) ==
            "Pointer_Stmt('POINTER', Pointer_Decl_List(',', (Pointer_Decl("
            "Name('a'), Deferred_Shape_Spec(None, None)), Name('b'))))")


def test_Pointer_Decl():  # R541

    cls = Pointer_Decl
    obj = cls('a(:)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(:)'
    assert (repr(obj) ==
            "Pointer_Decl(Name('a'), Deferred_Shape_Spec(None, None))")

    obj = cls('a(:,:)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(:, :)'


def test_Protected_Stmt():  # R542
    cls = Protected_Stmt
    obj = cls('protected a,b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PROTECTED :: a, b'
    assert (repr(obj) ==
            "Protected_Stmt('PROTECTED', Entity_Name_List(',', (Name('a'), "
            "Name('b'))))")

    obj = cls('protected ::a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PROTECTED :: a'
    assert repr(obj) == "Protected_Stmt('PROTECTED', Name('a'))"


def test_Save_Stmt():  # R543
    cls = Save_Stmt
    obj = cls('save')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SAVE'
    assert repr(obj) == "Save_Stmt('SAVE', None)"

    obj = cls('save a, b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SAVE :: a, b'
    assert (repr(obj) == "Save_Stmt('SAVE', "
            "Saved_Entity_List(',', (Name('a'), Name('b'))))")

    obj = cls('save :: /a/ , b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SAVE :: /a/, b'
    assert (repr(obj) ==
            "Save_Stmt('SAVE', Saved_Entity_List(',', (Saved_Entity('/', "
            "Name('a'), '/'), Name('b'))))")


def test_Saved_Entity():  # R544
    cls = Saved_Entity
    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'
    assert repr(obj) == "Name('a')"

    obj = cls('/a/')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '/a/'
    assert repr(obj) == "Saved_Entity('/', Name('a'), '/')"

# R545 is trivial


def test_Target_Stmt():  # R546
    cls = Target_Stmt
    obj = cls('target a, b(1000, 1000)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'TARGET :: a, b(1000, 1000)'

    obj = cls('target :: a, c')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'TARGET :: a, c'


def test_Value_Stmt():  # R547
    cls = Value_Stmt
    obj = cls('value a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'VALUE :: a'

    obj = cls('value:: a, c')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'VALUE :: a, c'


def test_Volatile_Stmt():  # R548
    cls = Volatile_Stmt
    obj = cls('volatile a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'VOLATILE :: a'

    obj = cls('volatile :: a, c')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'VOLATILE :: a, c'


def test_Implicit_Stmt():  # R549

    cls = Implicit_Stmt
    obj = cls('implicitnone')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'IMPLICIT NONE'
    assert repr(obj) == "Implicit_Stmt('NONE')"

    obj = cls('implicit real(a-d), double precision(r-t,x), type(a) (y-z)')
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'IMPLICIT REAL(A - D), DOUBLE PRECISION(R - T, X), '
            'TYPE(a)(Y - Z)')


def test_Implicit_Spec():  # R550

    cls = Implicit_Spec
    obj = cls('integer (a-z)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTEGER(A - Z)'
    assert (repr(obj) ==
            "Implicit_Spec(Intrinsic_Type_Spec('INTEGER', None), "
            "Letter_Spec('A', 'Z'))")

    obj = cls('double  complex (r,d-g)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DOUBLE COMPLEX(R, D - G)'


def test_Letter_Spec():  # R551

    cls = Letter_Spec
    obj = cls('a-z')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'A - Z'
    assert repr(obj) == "Letter_Spec('A', 'Z')"

    obj = cls('d')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'D'


def test_Namelist_Stmt():  # R552
    cls = Namelist_Stmt
    obj = cls('namelist / nlist / a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'NAMELIST /nlist/ a'

    obj = cls('namelist / nlist / a, /mlist/ b,c /klist/ d,e')
    assert str(obj) == 'NAMELIST /nlist/ a, /mlist/ b, c, /klist/ d, e'


def test_Equivalence_Stmt():  # R554

    cls = Equivalence_Stmt
    obj = cls('equivalence (a, b ,z)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'EQUIVALENCE(a, b, z)'
    assert (repr(obj) ==
            "Equivalence_Stmt('EQUIVALENCE', Equivalence_Set(Name('a'), "
            "Equivalence_Object_List(',', (Name('b'), Name('z')))))")

    obj = cls('equivalence (a, b ,z),(b,l)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'EQUIVALENCE(a, b, z), (b, l)'


def test_Common_Stmt():  # R557

    cls = Common_Stmt
    obj = cls('common a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'COMMON // a'
    assert repr(obj) == "Common_Stmt([(None, Name('a'))])"

    obj = cls('common // a,b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'COMMON // a, b'

    obj = cls('common /name/ a,b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'COMMON /name/ a, b'

    obj = cls('common /name/ a,b(4,5) // c, /ljuks/ g(2)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'COMMON /name/ a, b(4, 5) // c /ljuks/ g(2)'


def test_Common_Block_Object():  # R558

    cls = Common_Block_Object
    obj = cls('a(2)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(2)'
    assert (repr(obj) ==
            "Common_Block_Object(Name('a'), Explicit_Shape_Spec(None, "
            "Int_Literal_Constant('2', None)))")

    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'

#
# SECTION 6
#


def test_Substring():  # R609

    cls = Substring
    obj = cls('a(:)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(:)'
    assert repr(obj) == "Substring(Name('a'), Substring_Range(None, None))"

    obj = cls('a(1:2)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(1 : 2)'
    assert (repr(obj) ==
            "Substring(Name('a'), Substring_Range(Int_Literal_Constant('1',"
            " None), Int_Literal_Constant('2', None)))")


def test_Substring_Range():  # R611

    cls = Substring_Range
    obj = cls(':')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == ':'
    assert repr(obj) == "Substring_Range(None, None)"

    obj = cls('a+1:')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a + 1 :'

    obj = cls('a+1: c/foo(g)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a + 1 : c / foo(g)'

    obj = cls('a:b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a : b'
    assert repr(obj) == "Substring_Range(Name('a'), Name('b'))"

    obj = cls('a:')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a :'

    obj = cls(':b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == ': b'


def test_Data_Ref():  # R612

    cls = Data_Ref
    obj = cls('a%b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a % b'
    assert repr(obj) == "Data_Ref('%', (Name('a'), Name('b')))"

    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'


def test_Part_Ref():  # R613

    cls = Part_Ref
    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'


def test_Type_Param_Inquiry():  # R615

    cls = Type_Param_Inquiry
    obj = cls('a % b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a % b'
    assert repr(obj) == "Type_Param_Inquiry(Name('a'), '%', Name('b'))"


def test_Array_Section():  # R617

    cls = Array_Section
    obj = cls('a(:)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(:)'
    assert (repr(obj) ==
            "Array_Section(Name('a'), Substring_Range(None, None))")

    obj = cls('a(2:)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(2 :)'


def test_Section_Subscript():  # R619

    cls = Section_Subscript

    obj = cls('1:2')
    assert isinstance(obj, Subscript_Triplet), repr(obj)
    assert str(obj) == '1 : 2'

    obj = cls('zzz')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'zzz'


def test_Section_Subscript_List():  # R619-list

    cls = Section_Subscript_List
    obj = cls('a,2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a, 2'
    assert (repr(obj) ==
            "Section_Subscript_List(',', (Name('a'), Int_Literal_Constant("
            "'2', None)))")

    obj = cls('::1')
    assert isinstance(obj, Subscript_Triplet), repr(obj)
    assert str(obj) == ': : 1'

    obj = cls('::1, 3')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == ': : 1, 3'


def test_Subscript_Triplet():  # R620

    cls = Subscript_Triplet
    obj = cls('a:b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a : b'
    assert repr(obj) == "Subscript_Triplet(Name('a'), Name('b'), None)"

    obj = cls('a:b:1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a : b : 1'

    obj = cls(':')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == ':'

    obj = cls('::5')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == ': : 5'

    obj = cls(':5')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == ': 5'

    obj = cls('a+1 :')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a + 1 :'


def test_Allocate_Stmt():  # R623
    cls = Allocate_Stmt
    obj = cls('allocate(a,b)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'ALLOCATE(a, b)'

    obj = cls('allocate(real::a)')
    assert str(obj) == 'ALLOCATE(REAL::a)'

    obj = cls('allocate(real(kind=8)::a, stat=b, source=c//d)')
    assert (str(obj) ==
            'ALLOCATE(REAL(KIND = 8)::a, STAT = b, SOURCE = c // d)')


def test_Alloc_Opt():  # R624

    cls = Alloc_Opt
    obj = cls('stat=a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'STAT = a'
    assert repr(obj) == "Alloc_Opt('STAT', Name('a'))"


def test_Nullify_Stmt():  # R633

    cls = Nullify_Stmt
    obj = cls('nullify (a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'NULLIFY(a)'
    assert repr(obj) == "Nullify_Stmt('NULLIFY', Name('a'))"

    obj = cls('nullify (a,c)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'NULLIFY(a, c)'


def test_Deallocate_Stmt():  # R635
    cls = Deallocate_Stmt
    obj = cls('deallocate (a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DEALLOCATE(a)'

    obj = cls('deallocate (a,stat=b)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DEALLOCATE(a, STAT = b)'
    obj = cls('deallocate (a,c,stat=b,errmsg=d)')
    assert str(obj) == 'DEALLOCATE(a, c, STAT = b, ERRMSG = d)'

#
# SECTION 7
#


def test_Primary():  # R701

    cls = Primary
    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'

    obj = cls('(a)')
    assert isinstance(obj, Parenthesis), repr(obj)
    assert str(obj) == '(a)'

    obj = cls('1')
    assert isinstance(obj, Int_Literal_Constant), repr(obj)
    assert str(obj) == '1'

    obj = cls('1.')
    assert isinstance(obj, Real_Literal_Constant), repr(obj)
    assert str(obj) == '1.'

    obj = cls('(1, n)')
    assert isinstance(obj, Complex_Literal_Constant), repr(obj)
    assert str(obj) == '(1, n)'

    obj = cls('.true.')
    assert isinstance(obj, Logical_Literal_Constant), repr(obj)
    assert str(obj) == '.TRUE.'

    obj = cls('"hey a()c"')
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == '"hey a()c"'

    obj = cls('b"0101"')
    assert isinstance(obj, Binary_Constant), repr(obj)
    assert str(obj) == 'B"0101"'

    obj = cls('o"0107"')
    assert isinstance(obj, Octal_Constant), repr(obj)
    assert str(obj) == 'O"0107"'

    obj = cls('z"a107"')
    assert isinstance(obj, Hex_Constant), repr(obj)
    assert str(obj) == 'Z"A107"'

    obj = cls('a % b')
    assert isinstance(obj, Data_Ref), repr(obj)
    assert str(obj) == 'a % b'

    obj = cls('a(:)')
    assert isinstance(obj, Array_Section), repr(obj)
    assert str(obj) == 'a(:)'

    obj = cls('0.0E-1')
    assert isinstance(obj, Real_Literal_Constant), repr(obj)
    assert str(obj) == '0.0E-1'


def test_Parenthesis():  # R701.h

    cls = Parenthesis
    obj = cls('(a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(a)'
    assert repr(obj) == "Parenthesis('(', Name('a'), ')')"

    obj = cls('(a+1)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(a + 1)'

    obj = cls('((a))')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '((a))'

    obj = cls('(a+(a+c))')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(a + (a + c))'

    obj = cls('("a"+"c")')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '("a" + "c")'

    obj = cls('("a"+")")')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '("a" + ")")'

    obj = cls('''(')'+")")''')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '''(')' + ")")'''

    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('(a+b)*(c+d)')
    assert "Parenthesis: '(a+b)*(c+d)'" in str(excinfo)

    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('''()''')
    assert "Parenthesis: '()'" in str(excinfo)


def test_Level_1_Expr():  # R702

    cls = Level_1_Expr
    obj = cls('.hey. a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '.HEY. a'
    assert repr(obj) == "Level_1_Expr('.HEY.', Name('a'))"

    # assertRaises(NoMatchError,cls,'.not. a')

    obj = cls('.false.')
    assert isinstance(obj, Logical_Literal_Constant), repr(obj)


def test_Mult_Operand():  # R704

    cls = Mult_Operand
    obj = cls('a**b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a ** b'
    assert repr(obj) == "Mult_Operand(Name('a'), '**', Name('b'))"

    obj = cls('a**2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a ** 2'

    obj = cls('(a+b)**2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(a + b) ** 2'

    obj = cls('0.0E-1')
    assert isinstance(obj, Real_Literal_Constant), repr(obj)
    assert str(obj) == '0.0E-1'


def test_Add_Operand():  # R705

    cls = Add_Operand
    obj = cls('a*b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a * b'
    assert repr(obj) == "Add_Operand(Name('a'), '*', Name('b'))"

    obj = cls('a/b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a / b'

    obj = cls('a**b')
    assert isinstance(obj, Mult_Operand), repr(obj)
    assert str(obj) == 'a ** b'

    obj = cls('0.0E-1')
    assert isinstance(obj, Real_Literal_Constant), repr(obj)
    assert str(obj) == '0.0E-1'


def test_Level_2_Expr():  # R706

    cls = Level_2_Expr
    obj = cls('a+b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a + b'
    assert repr(obj) == "Level_2_Expr(Name('a'), '+', Name('b'))"

    obj = cls('a-b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a - b'

    obj = cls('a+b+c')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a + b + c'

    obj = cls('+a')
    assert isinstance(obj, Level_2_Unary_Expr), repr(obj)
    assert str(obj) == '+ a'

    obj = cls('+1')
    assert isinstance(obj, Level_2_Unary_Expr), repr(obj)
    assert str(obj) == '+ 1'

    obj = cls('+a+b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '+ a + b'

    obj = cls('0.0E-1')
    assert isinstance(obj, Real_Literal_Constant), repr(obj)
    assert str(obj) == '0.0E-1'


def test_Level_2_Unary_Expr():

    cls = Level_2_Unary_Expr
    obj = cls('+a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '+ a'
    assert repr(obj) == "Level_2_Unary_Expr('+', Name('a'))"

    obj = cls('-a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '- a'

    obj = cls('+1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '+ 1'

    obj = cls('0.0E-1')
    assert isinstance(obj, Real_Literal_Constant), repr(obj)
    assert str(obj) == '0.0E-1'


def test_Level_3_Expr():  # R710

    cls = Level_3_Expr
    obj = cls('a//b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a // b'
    assert repr(obj) == "Level_3_Expr(Name('a'), '//', Name('b'))"

    obj = cls('"a"//"b"')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '"a" // "b"'


def test_Level_4_Expr():  # R712

    cls = Level_4_Expr
    obj = cls('a.eq.b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a .EQ. b'
    assert repr(obj) == "Level_4_Expr(Name('a'), '.EQ.', Name('b'))"

    obj = cls('a.ne.b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a .NE. b'

    obj = cls('a.lt.b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a .LT. b'

    obj = cls('a.gt.b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a .GT. b'

    obj = cls('a.ge.b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a .GE. b'

    obj = cls('a==b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a == b'

    obj = cls('a/=b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a /= b'

    obj = cls('a<b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a < b'

    obj = cls('a<=b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a <= b'

    obj = cls('a>=b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a >= b'

    obj = cls('a>b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a > b'


def test_And_Operand():  # R714

    cls = And_Operand
    obj = cls('.not.a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '.NOT. a'
    assert repr(obj) == "And_Operand('.NOT.', Name('a'))"


def test_Or_Operand():  # R715

    cls = Or_Operand
    obj = cls('a.and.b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a .AND. b'
    assert repr(obj) == "Or_Operand(Name('a'), '.AND.', Name('b'))"


def test_Equiv_Operand():  # R716

    cls = Equiv_Operand
    obj = cls('a.or.b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a .OR. b'
    assert repr(obj) == "Equiv_Operand(Name('a'), '.OR.', Name('b'))"


def test_Level_5_Expr():  # R717

    cls = Level_5_Expr
    obj = cls('a.eqv.b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a .EQV. b'
    assert repr(obj) == "Level_5_Expr(Name('a'), '.EQV.', Name('b'))"

    obj = cls('a.neqv.b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a .NEQV. b'

    obj = cls('a.eq.b')
    assert isinstance(obj, Level_4_Expr), repr(obj)
    assert str(obj) == 'a .EQ. b'


def test_Expr():  # R722

    cls = Expr
    obj = cls('a .op. b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a .OP. b'
    assert repr(obj) == "Expr(Name('a'), '.OP.', Name('b'))"

    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'

    obj = cls('3.e2')
    assert isinstance(obj, Real_Literal_Constant), repr(obj)

    obj = cls('0.0E-1')
    assert isinstance(obj, Real_Literal_Constant), repr(obj)
    assert str(obj) == '0.0E-1'

    obj = cls('123')
    assert isinstance(obj, Int_Literal_Constant), repr(obj)
    assert str(obj) == '123'

    obj = cls('.false.')
    assert isinstance(obj, Logical_Literal_Constant), repr(obj)
    assert str(obj) == '.FALSE.'

    assertRaises(NoMatchError, Scalar_Int_Expr, 'a,b')


def test_Logical_Expr():  # R724
    cls = Logical_Expr
    obj = cls('(f0 .lt. f1) .and. abs(x1-x0) .gt. abs(x2) .or.  .not. root')
    assert isinstance(obj, Equiv_Operand), repr(obj)
    assert (str(obj) ==
            '(f0 .LT. f1) .AND. abs(x1 - x0) .GT. abs(x2) .OR. .NOT. root')


def test_Logical_Initialization_Expr():  # R733

    cls = Logical_Initialization_Expr
    obj = cls('.false.')
    assert isinstance(obj, Logical_Literal_Constant), repr(obj)
    assert str(obj) == '.FALSE.'


def test_Assignment_Stmt():  # R734

    cls = Assignment_Stmt
    obj = cls('a = b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a = b'
    assert repr(obj) == "Assignment_Stmt(Name('a'), '=', Name('b'))"

    obj = cls('a(3:4) = b+c')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(3 : 4) = b + c'

    obj = cls('a%c = b+c')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a % c = b + c'

    obj = cls('a = .FALSE.')
    assert isinstance(obj, cls), repr(obj)
    assert (repr(obj) ==
            "Assignment_Stmt(Name('a'), '=', Logical_Literal_Constant("
            "'.FALSE.', None))")

    obj = cls('a(n)(k:m) = 5')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a(n)(k : m) = 5'

    obj = cls('b = a + 1  d - 8')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'b = a + 1D-8'

    obj = cls('b = a + 1  d - 8 + 1.1e+3')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'b = a + 1D-8 + 1.1E+3'


def test_Pointer_Assignment_Stmt():  # R735
    cls = Pointer_Assignment_Stmt
    obj = cls('new_node % left => current_node')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'new_node % left => current_node'

    obj = cls('simple_name => target_structure % substruct % component')
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'simple_name => target_structure % substruct % component')

    for stmt in '''\
PTR => NULL()
ROW => MAT2D(N, :)
WINDOW => MAT2D(I - 1 : I + 1, J - 1 : J + 1)
POINTER_OBJECT => POINTER_FUNCTION(ARG_1, ARG_2)
EVERY_OTHER => VECTOR(1 : N : 2)
WINDOW2(0 :, 0 :) => MAT2D(ML : MU, NL : NU)
P => BESSEL
STRUCT % COMPONENT => BESSEL'''.split('\n'):
        obj = cls(stmt)
        assert isinstance(obj, cls), repr(obj)
        assert str(obj) == stmt


def test_Proc_Component_Ref():  # R741

    cls = Proc_Component_Ref
    obj = cls('a % b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a % b'
    assert repr(obj) == "Proc_Component_Ref(Name('a'), '%', Name('b'))"


def test_Where_Stmt():  # R743

    cls = Where_Stmt
    obj = cls('where (a) c=2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'WHERE (a) c = 2'
    assert (repr(obj) ==
            "Where_Stmt(Name('a'), Assignment_Stmt(Name('c'), '=', "
            "Int_Literal_Constant('2', None)))")


def test_Where_Construct():  # R745
    cls = Where_Construct
    obj = cls(get_reader('''\
    where (pressure <= 1.0)
    pressure = pressure + inc_pressure
    temp = temp - 5.0
    elsewhere
    raining = .true.
    end where
'''))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) == "WHERE (pressure <= 1.0)\n  "
            "pressure = pressure + inc_pressure\n  "
            "temp = temp - 5.0\n"
            "ELSEWHERE\n  raining = .TRUE.\nEND WHERE")

    obj = cls(get_reader('''\
    where (cond1)
    else    where (cond2)
    end where
'''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'WHERE (cond1)\nELSEWHERE(cond2)\nEND WHERE'

    obj = cls(get_reader('''\
    n:where (cond1)
    elsewhere (cond2) n
    else   where n
    end where n
'''))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) == "n:WHERE (cond1)\nELSEWHERE(cond2) n\n"
            "ELSEWHERE n\nEND WHERE n")

    obj = cls(get_reader('''\
    n:where (cond1)
    else where (cond2) n
    else where n
    end where n
'''))
    assert isinstance(obj, cls), repr(obj)
    print(str(obj))
    assert (str(obj) ==
            'n:WHERE (cond1)\nELSEWHERE(cond2) n\nELSEWHERE n\n'
            'END WHERE n')

    obj = cls(get_reader('''\
    n:where (me(:)=="hello")
    else where (me(:)=="goodbye") n
    else where n
    end where n
'''))
    print(str(obj))
    assert (str(obj) ==
            'n:WHERE (me(:) == "hello")\nELSEWHERE(me(:) == "goodbye") n\n'
            'ELSEWHERE n\n'
            'END WHERE n')


def test_Where_Construct_Stmt():  # R745

    cls = Where_Construct_Stmt
    obj = cls('where (a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'WHERE (a)'
    assert repr(obj) == "Where_Construct_Stmt(Name('a'))"


def test_Forall_Construct():  # R752
    cls = Forall_Construct
    obj = cls(get_reader('''\
    forall (i = 1:10, j = 1:10, b(i, j) /= 0.0)
      a(i, j) = real (i + j - 2)
      b(i, j) = a(i, j) + b(i, j) * real (i * j)
    end forall
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'FORALL(i = 1 : 10, j = 1 : 10, b(i, j) /= 0.0)\n'
            '  a(i, j) = real(i + j - 2)\n  b(i, j) = a(i, j) + '
            'b(i, j) * real(i * j)\nEND FORALL')

    obj = cls(get_reader('''\
    n: forall (x = 1:5:2, j = 1:4)
      a(x, j) = j
    end forall n
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'n:FORALL(x = 1 : 5 : 2, j = 1 : 4)\n  a(x, j) = j\nEND FORALL n')


def test_Forall_Header():  # R754
    cls = Forall_Header
    obj = cls('(n=1:2, a+1)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(n = 1 : 2, a + 1)'

    obj = cls('(n=1:2, m=1:x-1:z(a))')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(n = 1 : 2, m = 1 : x - 1 : z(a))'


def test_Forall_Triplet_Spec():  # R755

    cls = Forall_Triplet_Spec
    obj = cls('n = 1: 2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'n = 1 : 2'

    obj = cls('n = f(x): 2-b:a+1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'n = f(x) : 2 - b : a + 1'

#
# SECTION 8
#


def test_If_Construct():  # R802
    cls = If_Construct
    obj = cls(get_reader('''\
if (expr) then
  a = 1
end if
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'IF (expr) THEN\n  a = 1\nEND IF'

    obj = cls(get_reader('''\
name: if (expr) then
  a = 1
end if name
    '''))

    assert str(obj) == 'name:IF (expr) THEN\n  a = 1\nEND IF name'

    obj = cls(get_reader('''\
if (expr) then
  a = 1
  if (expr2) then
    a = 2
  endif
  a = 3
end if
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'IF (expr) THEN\n  a = 1\n  IF (expr2) THEN\n    a = 2\n'
            '  END IF\n  a = 3\nEND IF')

    obj = cls(get_reader('''\
if (expr) then
  a = 1
else if (expr2) then
  a = 2
end if
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'IF (expr) THEN\n  a = 1\nELSE IF (expr2) THEN\n  a = 2\nEND IF')

    obj = cls(get_reader('''\
if (expr) then
  a = 1
else
  a = 2
end if
    '''))
    assert str(obj) == 'IF (expr) THEN\n  a = 1\nELSE\n  a = 2\nEND IF'

    obj = cls(get_reader('''\
if (expr) then
  a = 1
else if (expr2) then
  a = 2
else
  a = 3
end if
    '''))
    assert (str(obj) ==
            'IF (expr) THEN\n  a = 1\nELSE IF (expr2) THEN\n  a = 2\n'
            'ELSE\n  a = 3\nEND IF')

    obj = cls(get_reader('''\
named: if (expr) then
  a = 1
else named
  a = 2
end if named
    '''))
    assert (str(obj) ==
            'named:IF (expr) THEN\n  a = 1\nELSE named\n  a = 2\nEND IF named')

    obj = cls(get_reader('''\
named: if (expr) then
  a = 1
  named2: if (expr2) then
    a = 2
  end if named2
end if named
'''))
    assert (str(obj) ==
            'named:IF (expr) THEN\n  a = 1\n  named2:IF (expr2) THEN\n'
            '    a = 2\n  END IF named2\nEND IF named')

    obj = cls(get_reader('''\
if (expr) then
  a = 1
else if (expr2) then
  a = 2
else if (expr3) then
  a = 3
end if
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'IF (expr) THEN\n  a = 1\nELSE IF (expr2) THEN\n  a = 2\n'
            'ELSE IF (expr3) THEN\n  a = 3\nEND IF')

    obj = cls(get_reader('''\
        if (dxmx .gt. 0d0) then
          diff = 0
          do  80  k = 1, n
   80     diff = max(diff,abs(xnew(k)-xin(k)))
          if (diff .gt. dxmx) then
            betx = dxmx/diff

            call awrit3(' broyj:  max shift = %1;3g'//
     .        ' is larger than dxmx = %1;3g.  Scale by %1;3g',
     .        ' ',80,i1mach(2),diff,dxmx,dxmx/diff)

            do  82  k = 1, n
   82       xnew(k) = xin(k) + betx*(xnew(k)-xin(k))
          endif
        endif'''))
    assert isinstance(obj, cls)


def test_if_nonblock_do():
    ''' Tests that conditional nonblock DO construct is parsed correctly '''
    cls = If_Construct

    obj = cls(get_reader('''\
if (expr) then
   do  20  i = 1, 3
     a = 1
     do  20  j = 1, 3
       a = 2
       do  20  k = 1, 3
         a = 3
20 rotm(i,j) = r2(j,i)
endif
'''))
    assert isinstance(obj, cls), repr(obj)
    assert len(obj.content) == 3, repr(obj)
    obj = obj.content[1]
    assert isinstance(obj, Action_Term_Do_Construct), repr(obj)
    assert str(obj) == (
        'DO 20 i = 1, 3\n  a = 1\n  DO 20 j = 1, 3\n    a = 2\n    '
        'DO 20 k = 1, 3\n      a = 3\n20 rotm(i, j) = r2(j, i)')

    obj = cls(get_reader('''\
if (expr) then
    do  50  i = n, m, -1
  50 call foo(a)
endif'''))
    assert isinstance(obj, cls), repr(obj)
    assert len(obj.content) == 3, repr(obj)
    obj = obj.content[1]
    assert isinstance(obj, Action_Term_Do_Construct), repr(obj)


def test_Case_Construct():  # R808
    cls = Case_Construct
    obj = cls(get_reader('''\
select case (n)
case (:-1)
  signum = -1
case (0)
  signum = 0
case (1:)
  signum = 1
end select
'''))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'SELECT CASE (n)\nCASE (: - 1)\n  signum = - 1\nCASE (0)\n'
            '  signum = 0\nCASE (1 :)\n  signum = 1\nEND SELECT')


def test_Case_Selector():  # R813
    cls = Case_Selector
    obj = cls('default')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DEFAULT'

    obj = cls('(2)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(2)'

    obj = cls('(2:3, c+2:, :-a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(2 : 3, c + 2 :, : - a)'


def test_Associate_Construct():  # R816
    cls = Associate_Construct
    obj = cls(get_reader('''\
ASSOCIATE ( Z => EXP(-(X**2+Y**2)) * COS(THETA) )
PRINT *, A+Z, A-Z
END ASSOCIATE
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'ASSOCIATE(Z => EXP(- (X ** 2 + Y ** 2)) * COS(THETA))\n'
            '  PRINT *, A + Z, A - Z\nEND ASSOCIATE')

    obj = cls(get_reader('''\
name:ASSOCIATE ( XC => AX%B(I,J)%C )
XC%DV = XC%DV + PRODUCT(XC%EV(1:N))
END ASSOCIATE name
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'name:ASSOCIATE(XC => AX % B(I, J) % C)\n  XC % DV = XC % DV + '
            'PRODUCT(XC % EV(1 : N))\nEND ASSOCIATE name')

    obj = cls(get_reader('''\
ASSOCIATE ( W => RESULT(I,J)%W, ZX => AX%B(I,J)%D, ZY => AY%B(I,J)%D )
W = ZX*X + ZY*Y
END ASSOCIATE
    '''))
    assert (str(obj) ==
            'ASSOCIATE(W => RESULT(I, J) % W, ZX => AX % B(I, J) % D, ZY => '
            'AY % B(I, J) % D)\n  W = ZX * X + ZY * Y\nEND ASSOCIATE')


def test_Select_Type_Construct():  # R821
    cls = Select_Type_Construct
    tree = cls(get_reader('''\
n:SELECT TYPE ( A => P_OR_C )
CLASS IS ( POINT )
PRINT *, A%X, A%Y ! This block gets executed
TYPE IS ( POINT_3D )
PRINT *, A%X, A%Y, A%Z
END SELECT n
    ''', ignore_comments=False))
    print(str(tree))
    assert (str(tree) == "n:SELECT TYPE(A=>P_OR_C)\n"
            "  CLASS IS (POINT)\n"
            "  PRINT *, A % X, A % Y\n"
            "  ! This block gets executed\n"
            "  TYPE IS (POINT_3D)\n"
            "  PRINT *, A % X, A % Y, A % Z\n"
            "END SELECT n")


def test_Select_Type_Stmt():  # R822
    cls = Select_Type_Stmt
    obj = cls('select type(a=>b)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SELECT TYPE(a=>b)'

    obj = cls('select type(a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SELECT TYPE(a)'


def test_Type_Guard_Stmt():  # R823
    cls = Type_Guard_Stmt
    obj = cls('type is (real*8)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'TYPE IS (REAL*8)'

    obj = cls('class is (mytype) name')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'CLASS IS (mytype) name'

    obj = cls('classdefault')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'CLASS DEFAULT'


def test_Block_Label_Do_Construct():  # pylint: disable=invalid-name
    ''' Tests that block labeled DO construct is parsed correctly (R826_1) '''
    cls = Block_Label_Do_Construct

    obj = cls(get_reader('''\
      do 12
        a = 1
 12   continue
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DO 12\n  a = 1\n12 CONTINUE'

    obj = cls(get_reader('''\
      foo: do 21, i=1,10
        a = 1
 21   end do foo
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'foo:DO 21 , i = 1, 10\n  a = 1\n21 END DO foo'

    obj = cls(get_reader('''
      do 51 while (a < 10)
        a = a + 1
 51   continue
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DO 51 WHILE (a < 10)\n  a = a + 1\n51 CONTINUE'

    obj = cls(get_reader('''
      do 52
        a = a + 1
        if (a > 10) exit
 52   continue
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DO 52\n  a = a + 1\n  IF (a > 10) EXIT\n52 CONTINUE'

    obj = cls(get_reader('''\
      do 12
        do 13
          a = 1
 13   continue
 12   continue
    '''))
    assert str(obj) == 'DO 12\n  DO 13\n    a = 1\n13 CONTINUE\n12 CONTINUE'
    assert len(obj.content) == 3, repr(len(obj.content))
    assert str(obj.content[1]) == 'DO 13\n  a = 1\n13 CONTINUE'

    obj = cls(get_reader('''
      do 52, i = 1,10
        do 53, while (j /= n)
        j = j + i
 53   continue
 52   continue
    '''))
    assert len(obj.content) == 3, repr(len(obj.content))
    assert str(obj) == ('DO 52 , i = 1, 10\n  DO 53 , WHILE (j /= n)\n'
                        '    j = j + i\n53 CONTINUE\n52 CONTINUE')
    assert str(obj.content[1]) == (
        'DO 53 , WHILE (j /= n)\n  j = j + i\n53 CONTINUE')


def test_Block_Nonlabel_Do_Construct():  # pylint: disable=invalid-name
    ''' Tests that block nonlabeled DO construct is parsed
    correctly (R826_2) '''
    cls = Block_Nonlabel_Do_Construct

    obj = cls(get_reader('''\
      do i=1,10
        a = 1
      end do
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DO i = 1, 10\n  a = 1\nEND DO'

    obj = cls(get_reader('''\
      do while (a < 10)
        a = a + 1
      end do
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DO WHILE (a < 10)\n  a = a + 1\nEND DO'

    obj = cls(get_reader('''
      do
        a = a - 1
        if (a < 10) exit
      end do
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DO\n  a = a - 1\n  IF (a < 10) EXIT\nEND DO'
    assert len(obj.content) == 4, repr(len(obj.content))
    assert str(obj.content[2]) == 'IF (a < 10) EXIT'

    obj = cls(get_reader('''\
      foo:do i=1,10
        a = 1
      end do foo
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'foo:DO i = 1, 10\n  a = 1\nEND DO foo'

    obj = cls(get_reader('''\
      foo:do while (a < 10)
        a = a + 1
      end do foo
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'foo:DO WHILE (a < 10)\n  a = a + 1\nEND DO foo'

    obj = cls(get_reader('''\
      do j=1,2
      foo:do i=1,10
        a = 1
      end do foo
      end do
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == ('DO j = 1, 2\n'
                        '  foo:DO i = 1, 10\n    a = 1\n  END DO foo\nEND DO')

    obj = cls(get_reader('''
      do while (j >= n)
      bar:do i=1,10
        a = i + j
      end do bar
      j = j - 1
      end do
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == ('DO WHILE (j >= n)\n'
                        '  bar:DO i = 1, 10\n    a = i + j\n  END DO bar\n'
                        '  j = j - 1\nEND DO')

    obj = cls(get_reader('''
      do, i = 1,10
      bar: do, while (j /= n)
        a = i - j
      end do bar
      end do
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == (
        'DO , i = 1, 10\n'
        '  bar:DO , WHILE (j /= n)\n    a = i - j\n  END DO bar\n'
        'END DO')
    assert len(obj.content) == 3, repr(len(obj.content))
    assert str(obj.content[1]) == (
        'bar:DO , WHILE (j /= n)\n  a = i - j\nEND DO bar')


def test_Label_Do_Stmt():  # pylint: disable=invalid-name
    ''' Tests that labeled DO statement is parsed correctly (R828) '''
    cls = Label_Do_Stmt
    obj = cls('do 12')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DO 12'
    assert repr(obj) == "Label_Do_Stmt(None, Label('12'), None)"


def test_Loop_Control():  # pylint: disable=invalid-name
    ''' Tests incorrect loop control constructs (R829). Correct loop
    control constructs are tested in test_Block_Label_Do_Construct()
    and test_Nonblock_Label_Do_Construct() '''
    cls = Loop_Control

    # More than one '=' in counter expression
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('j = 1 = 10')
    assert "Loop_Control: 'j = 1 = 10'" in str(excinfo)

    # Incorrect number of elements in counter expression
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('k = 10, -10, -2, -1')
    assert "Loop_Control: 'k = 10, -10, -2, -1'" in str(excinfo)
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('l = 5')
    assert "Loop_Control: 'l = 5'" in str(excinfo)


def test_Nonblock_Do_Construct():  # pylint: disable=invalid-name
    ''' Tests that nonblock DO construct is parsed correctly (R835) '''
    cls = Nonblock_Do_Construct
    obj = cls(get_reader('''\
      do  20,  i = 1, 3
 20     rotm(i,j) = r2(j,i)
    '''))
    assert isinstance(obj, Action_Term_Do_Construct), repr(obj)
    assert str(obj) == 'DO 20 , i = 1, 3\n20 rotm(i, j) = r2(j, i)'

    obj = cls(get_reader('''\
      do  20,  i = 1, 3
      k = 3
      do  20,  j = 1, 3
      l = 3
 20     rotm(i,j) = r2(j,i)
    '''))
    assert isinstance(obj, Action_Term_Do_Construct), repr(obj)
    assert str(obj) == (
        'DO 20 , i = 1, 3\n  k = 3\n  DO 20 , j = 1, 3\n    l = 3\n'
        '20 rotm(i, j) = r2(j, i)')

    obj = cls(get_reader('''\
      do  20  i = 1, 3
 20     rotm(i,j) = r2(j,i)
    '''))
    assert isinstance(obj, Action_Term_Do_Construct), repr(obj)
    assert str(obj) == 'DO 20 i = 1, 3\n20 rotm(i, j) = r2(j, i)'

    obj = cls(get_reader('''\
    do  50,  i = n, m, -1
  50 call foo(a)
    '''))
    assert isinstance(obj, Action_Term_Do_Construct), repr(obj)
    assert str(obj) == 'DO 50 , i = n, m, - 1\n50 CALL foo(a)'


def test_Continue_Stmt():  # R848

    cls = Continue_Stmt
    obj = cls('continue')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'CONTINUE'
    assert repr(obj) == "Continue_Stmt('CONTINUE')"


def test_Stop_Stmt():  # R849
    cls = Stop_Stmt
    obj = cls('stop')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'STOP'

    obj = cls('stop 123')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'STOP 123'

    obj = cls('stop   \'hey you\'')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == "STOP 'hey you'"

#
# SECTION 9
#


def test_Io_Unit():  # R901

    cls = Io_Unit
    obj = cls('*')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '*'

    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'


def test_read_stmt():  # R910
    ''' Check that we successfully parse various forms of READ statement '''
    cls = Read_Stmt
    obj = cls('read(123)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'READ(123)'

    obj = cls('read(123) a')
    assert str(obj) == 'READ(123) a'
    obj = cls('read(123) a(  2)')
    assert str(obj) == 'READ(123) a(2)'

    obj = cls('read*, a(  2), b')
    assert str(obj) == 'READ *, a(2), b'
    assert repr(obj) == (
        "Read_Stmt(None, Format('*'), Output_Item_List(',', (Part_Ref("
        "Name('a'), Int_Literal_Constant('2', None)), Name('b'))))")

    # With format specified by label number
    obj = cls("READ 13, a(2)")
    assert str(obj) == 'READ 13, a(2)'
    print(repr(obj))
    assert repr(obj) == ("Read_Stmt(None, Label('13'), Part_Ref(Name('a'), "
                         "Int_Literal_Constant('2', None)))")

    # If there is no preceding "FMT=" or "NML=" then there is no way of
    # knowing whether the second argument is a format string or a namelist
    # without determining the actual type of the argument.
    obj = cls('read(123, a_namelist_or_format)')
    assert str(obj) == "READ(123, a_namelist_or_format)"
    assert repr(obj) == ("Read_Stmt(Io_Control_Spec_List(',', "
                         "(Io_Control_Spec(None, Int_Literal_Constant('123', "
                         "None)), Io_Control_Spec(None, "
                         "Name('a_namelist_or_format')))), None, None)")


def test_write_stmt():  # R911
    ''' Tests for various forms of Write statement '''
    cls = Write_Stmt
    obj = cls('write (123)"hey"')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'WRITE(123) "hey"'
    assert repr(obj) == (
        "Write_Stmt(Io_Control_Spec_List(',', "
        "(Io_Control_Spec(None, Int_Literal_Constant('123', None)),)), "
        "Char_Literal_Constant('\"hey\"', None))")

    obj = cls('WRITE (*,"(I3)") my_int')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'WRITE(*, FMT = "(I3)") my_int'
    assert repr(obj) == (
        "Write_Stmt(Io_Control_Spec_List(',', "
        "(Io_Control_Spec(None, Io_Unit('*')), Io_Control_Spec('FMT', "
        "Char_Literal_Constant('\"(I3)\"', None)))), Name('my_int'))")

    obj = cls('WRITE (*,namtest)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'WRITE(*, namtest)'
    assert repr(obj) == (
        "Write_Stmt(Io_Control_Spec_List(',', "
        "(Io_Control_Spec(None, Io_Unit('*')), Io_Control_Spec(None, "
        "Name('namtest')))), None)")

    # Test when format specifier contains an '=' character
    iolist = Io_Control_Spec_List("*,'(5X,\"q_mesh =\",4F12.8)'")
    assert isinstance(iolist, Io_Control_Spec_List)
    obj = cls("WRITE(*,'(5X,\"q_mesh =\",1F12.8)') 1.d0")
    assert isinstance(obj, cls)
    assert repr(obj) == (
        "Write_Stmt(Io_Control_Spec_List(\',\', "
        "(Io_Control_Spec(None, Io_Unit(\'*\')), "
        "Io_Control_Spec(None, "
        "Char_Literal_Constant(\'\\\'(5X,\"q_mesh =\",1F12.8)\\\'\', "
        "None)))), Real_Literal_Constant(\'1.D0\', None))")
    obj = cls("WRITE(*,FMT='(5X,\"q_mesh =\",1F12.8)') 1.d0")
    assert isinstance(obj, cls)
    assert repr(obj) == (
        "Write_Stmt(Io_Control_Spec_List(\',\', "
        "(Io_Control_Spec(None, Io_Unit(\'*\')), "
        "Io_Control_Spec(\'FMT\', "
        "Char_Literal_Constant(\'\\\'(5X,\"q_mesh =\",1F12.8)\\\'\', "
        "None)))), Real_Literal_Constant(\'1.D0\', None))")


def test_Print_Stmt():  # R912

    cls = Print_Stmt
    obj = cls('print 123')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PRINT 123'
    assert repr(obj) == "Print_Stmt(Label('123'), None)"

    obj = cls('print *,"a=",a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PRINT *, "a=", a'


def test_Io_Control_Spec():  # R913

    cls = Io_Control_Spec
    obj = cls('end=123')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'END = 123'
    assert repr(obj) == "Io_Control_Spec('END', Label('123'))"


def test_Io_Control_Spec_List():  # R913-list
    ''' Test that we correctly parse and then generate various
    forms of IO-control specification lists '''
    cls = Io_Control_Spec_List
    obj = cls('end=123')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'END = 123'
    assert repr(obj) == \
        "Io_Control_Spec_List(',', (Io_Control_Spec('END', Label('123')),))"

    obj = cls('123')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '123'

    obj = cls('123,*')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '123, FMT = *'
    assert repr(obj) == ("Io_Control_Spec_List(',', (Io_Control_Spec(None, "
                         "Int_Literal_Constant('123', None)), "
                         "Io_Control_Spec('FMT', Format('*'))))")

    obj = cls('123,fmt=a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '123, FMT = a'
    assert repr(obj) == ("Io_Control_Spec_List(',', (Io_Control_Spec(None, "
                         "Int_Literal_Constant('123', None)), "
                         "Io_Control_Spec('FMT', Name('a'))))")

    obj = cls('123,nml=a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '123, NML = a'
    assert repr(obj) == ("Io_Control_Spec_List(',', (Io_Control_Spec(None, "
                         "Int_Literal_Constant('123', None)), "
                         "Io_Control_Spec('NML', Name('a'))))")

    obj = cls('123, "(I3)"')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '123, FMT = "(I3)"'

    obj = cls('123,a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '123, a'


def test_Format():  # R914

    cls = Format
    obj = cls('*')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '*'
    assert repr(obj) == "Format('*')"

    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'

    obj = cls('123')
    assert isinstance(obj, Label), repr(obj)
    assert str(obj) == '123'


def test_Io_Implied_Do():  # R917
    cls = Io_Implied_Do
    obj = cls('(a, i=1,2)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '(a, i = 1, 2)'

    obj = cls('((i+j,j=3,4,1), i=1,2)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '((i + j, j = 3, 4, 1), i = 1, 2)'


def test_Io_Implied_Do_Control():  # R919

    cls = Io_Implied_Do_Control
    obj = cls('i=1,2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'i = 1, 2'

    obj = cls('i=f(2),2-1,a+2')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'i = f(2), 2 - 1, a + 2'


def test_Wait_Stmt():  # R921

    cls = Wait_Stmt
    obj = cls('wait (123)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'WAIT(UNIT = 123)'


def test_Wait_Spec():  # R922

    cls = Wait_Spec
    obj = cls('123')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'UNIT = 123'
    assert (repr(obj) ==
            "Wait_Spec('UNIT', Int_Literal_Constant('123', None))")

    obj = cls('err=1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'ERR = 1'


def test_Backspace_Stmt():  # R923

    cls = Backspace_Stmt
    obj = cls('backspace 1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'BACKSPACE 1'

    obj = cls('backspace  (unit=1,err=2)')
    assert str(obj) == 'BACKSPACE(UNIT = 1, ERR = 2)'


def test_Endfile_Stmt():  # R924

    cls = Endfile_Stmt
    obj = cls('endfile 1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'ENDFILE 1'

    obj = cls('endfile  (unit=1,err=2)')
    assert str(obj) == 'ENDFILE(UNIT = 1, ERR = 2)'


def test_Rewind_Stmt():  # R925

    cls = Rewind_Stmt
    obj = cls('rewind 1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'REWIND 1'

    obj = cls('rewind  (unit=1,err=2)')
    assert str(obj) == 'REWIND(UNIT = 1, ERR = 2)'


def test_Position_Spec():  # R926

    cls = Position_Spec
    obj = cls('1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'UNIT = 1'
    obj = cls('unit=1')
    assert str(obj) == 'UNIT = 1'
    obj = cls('err=2')
    assert str(obj) == 'ERR = 2'
    obj = cls('iomsg=a')
    assert str(obj) == 'IOMSG = a'
    obj = cls('iostat=a')
    assert str(obj) == 'IOSTAT = a'


def test_Flush_Stmt():  # R927

    cls = Flush_Stmt
    obj = cls('flush 1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'FLUSH 1'

    obj = cls('flush  (unit=1,err=2)')
    assert str(obj) == 'FLUSH(UNIT = 1, ERR = 2)'


def test_Flush_Spec():  # R928

    cls = Flush_Spec
    obj = cls('1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'UNIT = 1'
    obj = cls('unit=1')
    assert str(obj) == 'UNIT = 1'
    obj = cls('err=2')
    assert str(obj) == 'ERR = 2'
    obj = cls('iomsg=a')
    assert str(obj) == 'IOMSG = a'
    obj = cls('iostat=a')
    assert str(obj) == 'IOSTAT = a'


def test_Inquire_Stmt():  # R929
    ''' Tests for the Inquire statement '''
    cls = Inquire_Stmt
    obj = cls('inquire(1,file=a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INQUIRE(UNIT = 1, FILE = a)'
    obj = cls('inquire(iolength=n) a, b')
    assert str(obj) == 'INQUIRE(IOLENGTH=n) a, b'
    obj = cls('inquire(unit=get_unit, opened=llopn)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INQUIRE(UNIT = get_unit, OPENED = llopn)'


def test_Inquire_Spec():  # R930
    ''' Test that we recognise the various possible forms of
    entries in an inquire list '''
    cls = Inquire_Spec
    obj = cls('1')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'UNIT = 1'
    obj = cls('file=fn')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'FILE = fn'

    obj = cls('access=a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'ACCESS = a'

    obj = cls('opened=a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'OPENED = a'

    obj = cls('sequential=a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SEQUENTIAL = a'

    obj = cls('direct=a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'DIRECT = a'


def test_Inquire_Spec_List():  # pylint: disable=invalid-name
    ''' Test that we recognise the various possible forms of
    inquire list - R930
    '''
    # Inquire_Spec_List is generated at runtime in Fortran2003.py
    cls = Inquire_Spec_List

    obj = cls('unit=23, file="a_file.dat"')
    assert isinstance(obj, cls)
    assert str(obj) == 'UNIT = 23, FILE = "a_file.dat"'

    # Invalid list (afile= instead of file=)
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('unit=23, afile="a_file.dat"')
    assert "NoMatchError: Inquire_Spec_List: 'unit=23, afile=" in str(excinfo)


def test_Open_Stmt():
    ''' Check that we correctly parse and re-generate the various forms
    of OPEN statement (R904)'''
    cls = Open_Stmt
    obj = cls("open(23, file='some_file.txt')")
    assert isinstance(obj, cls)
    assert str(obj) == "OPEN(UNIT = 23, FILE = 'some_file.txt')"
    obj = cls("open(unit=23, file='some_file.txt')")
    assert isinstance(obj, cls)
    assert str(obj) == "OPEN(UNIT = 23, FILE = 'some_file.txt')"


def test_Connect_Spec():
    ''' Tests for individual elements of Connect_Spec (R905) '''
    cls = Connect_Spec
    # Incorrect name for a member of the list
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls("afile='a_file.dat'")
    assert 'NoMatchError: Connect_Spec: \'afile=' in str(excinfo)


def test_Connect_Spec_List():  # pylint: disable=invalid-name
    '''
    Check that we correctly parse the various valid forms of
    connect specification (R905)
    '''
    cls = Connect_Spec_List
    obj = cls("22, access='direct'")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, ACCESS = 'direct'"

    obj = cls("22, action='read'")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, ACTION = 'read'"

    obj = cls("22, asynchronous='YES'")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, ASYNCHRONOUS = 'YES'"

    obj = cls("22, blank='NULL'")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, BLANK = 'NULL'"

    obj = cls("22, decimal='COMMA'")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, DECIMAL = 'COMMA'"

    obj = cls("22, delim='APOSTROPHE'")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, DELIM = 'APOSTROPHE'"

    obj = cls("22, err=109")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, ERR = 109"

    obj = cls("22, encoding='DEFAULT'")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, ENCODING = 'DEFAULT'"

    obj = cls("22, file='a_file.dat'")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat'"

    obj = cls("22, file='a_file.dat', form='FORMATTED'")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', FORM = 'FORMATTED'"

    obj = cls("22, file='a_file.dat', iomsg=my_string")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', IOMSG = my_string"

    obj = cls("22, file='a_file.dat', iostat=ierr")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', IOSTAT = ierr"

    obj = cls("22, file='a_file.dat', pad='YES'")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', PAD = 'YES'"

    obj = cls("22, file='a_file.dat', position='APPEND'")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', POSITION = 'APPEND'"

    obj = cls("22, file='a_file.dat', recl=100")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', RECL = 100"

    obj = cls("22, file='a_file.dat', round='UP'")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', ROUND = 'UP'"

    obj = cls("22, file='a_file.dat', sign='PLUS'")
    assert isinstance(obj, cls)
    assert str(obj) == "UNIT = 22, FILE = 'a_file.dat', SIGN = 'PLUS'"

    obj = cls("22, file='a_file.dat', sign='PLUS', status='OLD'")
    assert isinstance(obj, cls)
    assert str(obj) == ("UNIT = 22, FILE = 'a_file.dat', SIGN = 'PLUS', "
                        "STATUS = 'OLD'")

    # Incorrect name for a member of the list
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls("unit=22, afile='a_file.dat', sign='PLUS', status='OLD'")
    assert 'NoMatchError: Connect_Spec_List: \'unit=22, afile=' in str(excinfo)

#
# SECTION 10
#


def test_Format_Stmt():  # R1001
    cls = Format_Stmt
    obj = cls('format (3f9.4)')
    assert isinstance(obj, cls), repr(type(a))
    assert str(obj) == 'FORMAT(3F9.4)'
    obj = cls("format (' ',3f9.4)")
    assert isinstance(obj, cls), repr(type(a))
    assert str(obj) == "FORMAT(' ', 3F9.4)"

    obj = cls('format(i6,f12.6,2x,f12.6)')
    assert isinstance(obj, cls), repr(type(a))
    assert str(obj) == 'FORMAT(I6, F12.6, 2X, F12.6)'

    obj = cls("format(' Enter smth',$)")
    assert isinstance(obj, cls), repr(type(a))
    assert str(obj) == "FORMAT(' Enter smth', $)"

    obj = cls("format(/'a' /'b')")
    assert isinstance(obj, cls), repr(type(a))
    assert str(obj) == "FORMAT(/, 'a', /, 'b')"

    obj = cls("format('a:':' b')")
    assert isinstance(obj, cls), repr(type(a))
    assert str(obj) == "FORMAT('a:', :, ' b')"

    return
    obj = cls("format('text=','  '")
    assert str(obj) == ''


def test_Format_Specification():  # R1002
    cls = Format_Specification
    obj = cls('(3f9.4, 2f8.1)')
    assert isinstance(obj, cls), repr(type(a))
    assert str(obj) == '(3F9.4, 2F8.1)'

    obj = cls("(' ', 2f8.1)")
    assert isinstance(obj, cls), repr(type(a))
    assert str(obj) == "(' ', 2F8.1)"


def test_Format_Item():  # R1003
    cls = Format_Item
    obj = cls('3f9.4')
    assert isinstance(obj, cls), repr(type(a))
    assert str(obj) == '3F9.4'

    obj = cls("' '")
    assert isinstance(obj, Char_Literal_Constant), repr(type(a))
    assert str(obj) == "' '"

    obj = cls('i4/')
    assert isinstance(obj, Format_Item_C1002), repr(type(a))
    assert str(obj) == 'I4, /'

    obj = cls('3f12.6/')
    assert str(obj) == '3F12.6, /'

    obj = cls('3d12.6/')
    assert str(obj) == '3D12.6, /'

    # D specifier must be Dw.d so must have a decimal point
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('3d12/')

    obj = cls('3e12.6/')
    assert str(obj) == '3E12.6, /'

    obj = cls('3e12.6e2/')
    assert str(obj) == '3E12.6E2, /'

    # Scientific format
    obj = cls('3es12.6/')
    assert str(obj) == '3ES12.6, /'

    # Engineering format
    obj = cls('3en12.6/')
    assert str(obj) == '3EN12.6, /'

    # Must have a decimal point
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('3en12/')

    # Engineering format specifying number of digits in exponent
    obj = cls('3en12.6e3/')
    assert str(obj) == '3EN12.6E3, /'

    obj = cls("/' '")
    assert str(obj) == "/, ' '"

    obj = cls("' '/")
    assert str(obj) == "' ', /"

    obj = cls("' '/' '")
    assert str(obj) == "' ', /, ' '"

    obj = cls("'(5X,\"q_mesh =\",4F12.8)'")
    assert isinstance(obj, Char_Literal_Constant)


def test_Edit_Desc():
    ''' Tests for matching Edit Descriptors '''
    cls = Data_Edit_Desc
    obj = cls('I3')
    assert str(obj) == 'I3'

    obj = cls('I3.2')
    assert str(obj) == 'I3.2'

    obj = cls('O3.2')
    assert str(obj) == 'O3.2'

    obj = cls('Z3.2')
    assert str(obj) == 'Z3.2'

    obj = cls('L3')
    assert str(obj) == 'L3'

    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('L3.2')
    assert "NoMatchError: Data_Edit_Desc: 'L3.2'" in str(excinfo)

    obj = cls('A3')
    assert str(obj) == 'A3'

    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('A3.2')
    assert "NoMatchError: Data_Edit_Desc: 'A3.2'" in str(excinfo)

    obj = cls("DT'a_name'")
    assert str(obj) == "DT'a_name'"

    obj = cls("DT'a_name'(3,-2)")
    assert str(obj) == "DT'a_name'(3, -2)"

    with pytest.raises(NoMatchError) as excinfo:
        _ = cls("DT'a_name'()")
    assert '''Data_Edit_Desc: \'DT\'a_name\'()\'''' in str(excinfo)


def test_Format_Item_List():
    cls = Format_Item_List
    obj = cls('3f9.4')
    assert isinstance(obj, Format_Item), repr(type(obj))
    assert str(obj) == '3F9.4'

    obj = cls('3f9.4, 2f8.1')
    assert isinstance(obj, Format_Item_List), repr(type(obj))
    assert str(obj) == '3F9.4, 2F8.1'

    obj = cls("' ', 2f8.1")
    assert isinstance(obj, Format_Item_List), repr(type(obj))
    assert str(obj) == "' ', 2F8.1"

    obj = cls("' ', ' '")
    assert str(obj) == "' ', ' '"

    obj = cls("3(3f8.2, :), (A)")
    assert str(obj) == "3(3F8.2, :), (A)"

#
# SECTION 11
#


def test_Main_Program():  # R1101
    cls = Main_Program
    obj = cls(get_reader('''\
program a
end
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PROGRAM a\nEND PROGRAM a'

    obj = cls(get_reader('''\
program a
  real b
  b = 1
  contains
  subroutine foo
  end
end
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'PROGRAM a\n  REAL :: b\n  b = 1\n  CONTAINS\n  SUBROUTINE foo\n'
            '  END SUBROUTINE foo\nEND PROGRAM a')

    obj = Main_Program0(get_reader('''\
end
    '''))
    assert isinstance(obj, Main_Program0), repr(obj)
    assert str(obj) == 'END PROGRAM'

    obj = Main_Program0(get_reader('''\
contains
  function foo()
  end
end
    '''))
    assert isinstance(obj, Main_Program0), repr(obj)
    assert str(obj) == 'CONTAINS\nFUNCTION foo()\nEND FUNCTION\nEND PROGRAM'


def test_Module():  # R1104
    cls = Module
    obj = cls(get_reader('''\
module m
end
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'MODULE m\nEND MODULE m'

    obj = cls(get_reader('''\
module m
type a
end type
type b
end type b
end
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'MODULE m\n  TYPE :: a\n  END TYPE a\n  TYPE :: b\n  END TYPE b'
            '\nEND MODULE m')


def test_Module_Subprogram_Part():  # R1107
    cls = Module_Subprogram_Part
    obj = cls(get_reader('''\
contains
  subroutine foo(a)
  real a
  a = 1.0
  end
    ''', isfree=True))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) == 'CONTAINS\nSUBROUTINE foo(a)\n  REAL :: a'
            '\n  a = 1.0\nEND SUBROUTINE foo')


def test_Use_Stmt():  # pylint: disable=invalid-name
    ''' Tests that USE statement is parsed correctly (R1109) '''
    cls = Use_Stmt
    obj = cls('use a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'USE a'
    assert repr(obj) == "Use_Stmt(None, None, Name('a'), '', None)"

    obj = cls('use :: a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'USE :: a'
    assert repr(obj) == "Use_Stmt(None, '::', Name('a'), '', None)"

    obj = cls('use a, only: b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'USE a, ONLY: b'
    assert repr(obj) == (
        "Use_Stmt(None, None, Name('a'), ', ONLY:', Name('b'))")

    obj = cls('use :: a, only: b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'USE :: a, ONLY: b'
    assert repr(obj) == (
        "Use_Stmt(None, '::', Name('a'), ', ONLY:', Name('b'))")

    obj = cls('use a, ONLY : b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'USE a, ONLY: b'
    assert repr(obj) == (
        "Use_Stmt(None, None, Name('a'), ', ONLY:', Name('b'))")

    obj = cls('use, intrinsic :: a, ONLY: b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'USE, INTRINSIC :: a, ONLY: b'
    assert repr(obj) == (
        "Use_Stmt(Module_Nature('INTRINSIC'), '::', Name('a'), "
        "', ONLY:', Name('b'))")

    obj = cls('use, non_intrinsic :: a, ONLY: b, c, d')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'USE, NON_INTRINSIC :: a, ONLY: b, c, d'
    assert repr(obj) == (
        "Use_Stmt(Module_Nature('NON_INTRINSIC'), '::', Name('a'), "
        "', ONLY:', Only_List(',', (Name('b'), Name('c'), Name('d'))))")

    obj = cls('use a, c=>d')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'USE a, c => d'
    assert repr(obj) == (
        "Use_Stmt(None, None, Name('a'), "
        "',', Rename(None, Name('c'), Name('d')))")

    obj = cls('use :: a, operator(.hey.)=>operator(.hoo.)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'USE :: a, OPERATOR(.HEY.) => OPERATOR(.HOO.)'
    assert repr(obj) == (
        "Use_Stmt(None, '::', Name('a'), ',', "
        "Rename('OPERATOR', Defined_Op('.HEY.'), Defined_Op('.HOO.')))")

    obj = cls('use, intrinsic :: a, operator(.hey.)=>operator(.hoo.), c=>g')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == (
        'USE, INTRINSIC :: a, OPERATOR(.HEY.) => OPERATOR(.HOO.), c => g')
    assert repr(obj) == (
        "Use_Stmt(Module_Nature('INTRINSIC'), '::', Name('a'), "
        "',', Rename_List(',', ("
        "Rename('OPERATOR', Defined_Op('.HEY.'), Defined_Op('.HOO.')), "
        "Rename(None, Name('c'), Name('g')))))")

    obj = cls('use, non_intrinsic :: a, ONLY: b => c')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'USE, NON_INTRINSIC :: a, ONLY: b => c'
    assert repr(obj) == (
        "Use_Stmt(Module_Nature('NON_INTRINSIC'), '::', Name('a'), "
        "', ONLY:', Rename(None, Name('b'), Name('c')))")

    # Checks that no match is found for incorrect 'USE' statement contructs
    # Incorrect 'USE' statement
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('8se')
    assert "Use_Stmt: '8se'" in str(excinfo)

    # Empty string after 'USE'
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('use')
    assert "Use_Stmt: 'use'" in str(excinfo)

    # No separation between 'USE' statement and its specifiers
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('usemodulename')
    assert "Use_Stmt: 'usemodulename'" in str(excinfo)

    # Missing Module_Nature between ',' and '::'
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('use, ::')
    assert "Use_Stmt: 'use, ::'" in str(excinfo)

    # No Module_Name after 'USE, Module_Nature ::'
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('use, intrinsic ::')
    assert "Use_Stmt: 'use, intrinsic ::'" in str(excinfo)

    # Missing '::' after Module_Nature
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('use, intrinsic a')
    assert "Use_Stmt: 'use, intrinsic a'" in str(excinfo)

    # Missing Module_Name before Only_List
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('use , only: b')
    assert "Use_Stmt: 'use , only: b'" in str(excinfo)

    # Missing 'ONLY' specification after 'USE Module_Name,'
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('use a,')
    assert "Use_Stmt: 'use a,'" in str(excinfo)

    # Missing ':' after ', ONLY' specification
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('use a, only b')
    assert "Use_Stmt: 'use a, only b" in str(excinfo)

    # Missing Only_List/Rename_List after 'USE Module_Name, ONLY:'
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('use a, only:')
    assert "Use_Stmt: 'use a, only:" in str(excinfo)


def test_Module_Nature():  # pylint: disable=invalid-name
    ''' Tests that a module nature statement is parsed correctly
    (INTRINSIC or NON_INTRINSIC allowed, R1110) '''
    cls = Module_Nature
    obj = cls('intrinsic')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTRINSIC'
    assert repr(obj) == "Module_Nature('INTRINSIC')"

    obj = cls('non_intrinsic')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'NON_INTRINSIC'
    assert repr(obj) == "Module_Nature('NON_INTRINSIC')"

    # Incorrect module nature
    with pytest.raises(NoMatchError) as excinfo:
        _ = cls('other_nature')
    assert "Module_Nature: 'other_nature'" in str(excinfo)


def test_Rename():  # R1111
    cls = Rename
    obj = cls('a=>b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a => b'

    obj = cls('operator(.foo.)=>operator(.bar.)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'OPERATOR(.FOO.) => OPERATOR(.BAR.)'


def test_Block_Data():  # R1116
    cls = Block_Data
    obj = cls(get_reader('''\
block data a
real b
end block data
    '''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'BLOCK DATA a\n  REAL :: b\nEND BLOCK DATA a'

#
# SECTION 12
#


def test_Interface_Block():  # R1201
    cls = Interface_Block
    obj = cls(get_reader('''\
interface
end interface'''))
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTERFACE\nEND INTERFACE'

    obj = cls(get_reader('''\
abstract interface
procedure a
module procedure b,c
end interface
'''))
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'ABSTRACT INTERFACE\n  MODULE PROCEDURE a\n  MODULE PROCEDURE b, '
            'c\nEND INTERFACE')


def test_Interface_Specification():  # R1202
    cls = Interface_Specification
    obj = cls(get_reader('''\
    function foo()
    end
    '''))
    assert isinstance(obj, Function_Body), repr(obj)
    assert str(obj) == 'FUNCTION foo()\nEND FUNCTION'


def test_Interface_Stmt():  # R1203
    cls = Interface_Stmt
    obj = cls('interface')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTERFACE'

    obj = cls('interface assignment(=)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTERFACE ASSIGNMENT(=)'

    obj = cls('abstract interface')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'ABSTRACT INTERFACE'


def test_End_Interface_Stmt():  # R1204
    cls = End_Interface_Stmt
    obj = cls('end interface')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'END INTERFACE'

    obj = cls('end interface read(formatted)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'END INTERFACE READ(FORMATTED)'

    obj = cls('end interface assignment(=)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'END INTERFACE ASSIGNMENT(=)'


def test_Interface_Body():  # R1205
    cls = Interface_Body
    obj = cls(get_reader('''\
subroutine foo
end subroutine foo
'''))
    assert isinstance(obj, Subroutine_Body), repr(obj)
    assert str(obj) == 'SUBROUTINE foo\nEND SUBROUTINE foo'

    obj = cls(get_reader('''\
function foo(a) result(c)
  real a, c
end
'''))
    assert isinstance(obj, Function_Body), repr(obj)
    assert (str(obj) ==
            'FUNCTION foo(a) RESULT(c)\n  REAL :: a, c\nEND FUNCTION')


def test_Subroutine_Body():
    pass


def test_Function_Body():
    pass


def test_Procedure_Stmt():  # R1206
    cls = Procedure_Stmt
    obj = cls('module procedure a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'MODULE PROCEDURE a'

    obj = cls('procedure a, b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'MODULE PROCEDURE a, b'


def test_Generic_Spec():  # R1207
    cls = Generic_Spec
    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'
    obj = cls('read(formatted)')
    assert isinstance(obj, Dtio_Generic_Spec), repr(obj)
    assert str(obj) == 'READ(FORMATTED)'

    obj = cls('assignment ( = )')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'ASSIGNMENT(=)'

    return  # TODO
    obj = cls('operator(.foo.)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'OPERATOR(.foo.)'


def test_Dtio_Generic_Spec():  # R1208
    cls = Dtio_Generic_Spec
    obj = cls('read   ( formatted )')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'READ(FORMATTED)'

    obj = cls('write ( formatted )')
    assert str(obj) == 'WRITE(FORMATTED)'
    obj = cls('read   ( unformatted )')
    assert str(obj) == 'READ(UNFORMATTED)'
    obj = cls('write ( unformatted )')
    assert str(obj) == 'WRITE(UNFORMATTED)'


def test_Import_Stmt():  # R1209
    cls = Import_Stmt
    obj = cls('import :: a, b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'IMPORT :: a, b'

    obj = cls('import a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'IMPORT :: a'


def test_External_Stmt():  # R1210
    cls = External_Stmt
    obj = cls('external :: a, b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'EXTERNAL :: a, b'

    obj = cls('external a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'EXTERNAL :: a'


def test_Procedure_Declaration_Stmt():  # R1211
    cls = Procedure_Declaration_Stmt
    obj = cls('procedure () a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PROCEDURE() a'

    obj = cls('procedure (n) a')
    assert str(obj) == 'PROCEDURE(n) a'

    obj = cls('procedure (real*8) a')
    assert str(obj) == 'PROCEDURE(REAL*8) a'

    obj = cls('procedure (real(kind=8)) a')
    assert str(obj) == 'PROCEDURE(REAL(KIND = 8)) a'

    obj = cls('procedure (real*8) :: a')
    assert str(obj) == 'PROCEDURE(REAL*8) a'

    obj = cls('procedure (real*8), intent(in), bind(c) :: a, b')
    assert str(obj) == 'PROCEDURE(REAL*8), INTENT(IN), BIND(C) :: a, b'


@pytest.mark.parametrize(
    'procedure_attribute_input,expected_class,expected_string',
    [('private', Access_Spec, 'PRIVATE'),
     ('public', Access_Spec, 'PUBLIC'),
     ('bind(c)', Language_Binding_Spec, 'BIND(C)'),
     ('bind(c, name="foo")', Language_Binding_Spec, 'BIND(C, NAME = "foo")'),
     ('intent(in)', Proc_Attr_Spec, 'INTENT(IN)'),
     ('intent(out)', Proc_Attr_Spec, 'INTENT(OUT)'),
     ('intent(inout)', Proc_Attr_Spec, 'INTENT(INOUT)'),
     ('optional', Proc_Attr_Spec, 'OPTIONAL'),
     ('pointer', Proc_Attr_Spec, 'POINTER'),
     ('protected', Proc_Attr_Spec, 'PROTECTED'),
     ('save', Proc_Attr_Spec, 'SAVE')])
def test_Proc_Attr_Spec(procedure_attribute_input, expected_class,
                        expected_string):
    '''
    Tests the procedure attribute specification as outlined in #R1213 of
    ISO/IEC 1539-1:2010.
    '''
    unit_under_test = Proc_Attr_Spec

    result = unit_under_test(procedure_attribute_input)
    assert isinstance(result, expected_class)
    assert str(result) == expected_string


def test_Proc_Decl():  # R1214

    cls = Proc_Decl
    obj = cls('a => NULL')
    assert isinstance(obj, cls)
    assert str(obj) == 'a => NULL'

    obj = cls('a')
    assert isinstance(obj, Name), repr(type(a))
    assert str(obj) == 'a'


def test_Intrinsic_Stmt():  # R1216

    cls = Intrinsic_Stmt
    obj = cls('intrinsic :: a, b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTRINSIC :: a, b'
    obj = cls('intrinsic a, b')
    assert str(obj) == 'INTRINSIC :: a, b'

    obj = cls('intrinsic a')
    assert str(obj) == 'INTRINSIC :: a'


def test_Function_Reference():  # R1217

    cls = Function_Reference
    obj = cls('f()')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'f()'
    assert repr(obj) == "Function_Reference(Name('f'), None)"

    obj = cls('f(2,k=1,a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'f(2, k = 1, a)'


def test_Call_Stmt():  # R1218

    cls = Call_Stmt
    obj = cls('call a')
    assert isinstance(obj, cls)
    assert str(obj) == 'CALL a'

    obj = cls('call a()')
    assert str(obj) == 'CALL a'

    obj = cls('call a(b,c)')
    assert str(obj) == 'CALL a(b, c)'


def test_Procedure_Designator():  # R1219

    cls = Procedure_Designator
    obj = cls('a%b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a % b'
    assert repr(obj) == "Procedure_Designator(Name('a'), '%', Name('b'))"


def test_Actual_Arg_Spec():  # R1220

    cls = Actual_Arg_Spec
    obj = cls('k=a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'k = a'
    assert repr(obj) == "Actual_Arg_Spec(Name('k'), Name('a'))"

    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'


def test_Actual_Arg_Spec_List():

    cls = Actual_Arg_Spec_List
    obj = cls('a,b')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'a, b'
    assert repr(obj) == "Actual_Arg_Spec_List(',', (Name('a'), Name('b')))"

    obj = cls('a = k')
    assert isinstance(obj, Actual_Arg_Spec), repr(obj)
    assert str(obj) == 'a = k'

    obj = cls('a = k,b')
    assert isinstance(obj, Actual_Arg_Spec_List), repr(obj)
    assert str(obj) == 'a = k, b'

    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'


def test_Alt_Return_Spec():  # R1222

    cls = Alt_Return_Spec
    obj = cls('* 123')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '*123'
    assert repr(obj) == "Alt_Return_Spec(Label('123'))"


def test_Function_Subprogram():  # R1223

    reader = get_reader('''\
    function foo()
    end function foo''')
    cls = Function_Subprogram
    obj = cls(reader)
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'FUNCTION foo()\nEND FUNCTION foo'
    assert (repr(obj) ==
            "Function_Subprogram(Function_Stmt(None, Name('foo'), None, None),"
            " End_Function_Stmt('FUNCTION', Name('foo')))")

    reader = get_reader('''\
    pure real function foo(a) result(b) bind(c)
    integer a
    end function foo''')
    cls = Function_Subprogram
    obj = cls(reader)
    assert isinstance(obj, cls), repr(obj)
    assert (str(obj) ==
            'PURE REAL FUNCTION foo(a) RESULT(b) BIND(C)\n  INTEGER :: '
            'a\nEND FUNCTION foo')


def test_Function_Stmt():  # R1224
    cls = Function_Stmt
    obj = cls('function foo()')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'FUNCTION foo()'
    assert repr(obj) == "Function_Stmt(None, Name('foo'), None, None)"

    obj = cls('function foo(a,b)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'FUNCTION foo(a, b)'
    assert (repr(obj) ==
            "Function_Stmt(None, Name('foo'), Dummy_Arg_List(',', "
            "(Name('a'), Name('b'))), None)")

    obj = cls('function foo(a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'FUNCTION foo(a)'

    obj = cls('real function foo(a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'REAL FUNCTION foo(a)'

    obj = cls('real recursive function foo(a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'REAL RECURSIVE FUNCTION foo(a)'

    obj = cls('real function foo(a) bind(c)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'REAL FUNCTION foo(a) BIND(C)'

    obj = cls('real function foo(a) result (b)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'REAL FUNCTION foo(a) RESULT(b)'

    obj = cls('real function foo(a) bind(c) result(b)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'REAL FUNCTION foo(a) RESULT(b) BIND(C)'


def test_Dummy_Arg_Name():  # R1226
    cls = Dummy_Arg_Name
    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'


def test_Prefix():  # R1227

    cls = Prefix
    obj = cls('pure  recursive')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PURE RECURSIVE'
    assert (repr(obj) ==
            "Prefix(' ', (Prefix_Spec('PURE'), Prefix_Spec('RECURSIVE')))")

    obj = cls('integer * 2 pure')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'INTEGER*2 PURE'


@pytest.mark.parametrize(
    'procedure_prefix_input,expected_class,expected_string',
    [('integer', Intrinsic_Type_Spec, 'INTEGER'),
     ('integer * 2', Intrinsic_Type_Spec, 'INTEGER*2'),
     ('real', Intrinsic_Type_Spec, 'REAL'),
     ('double complex', Intrinsic_Type_Spec, 'DOUBLE COMPLEX'),
     ('complex', Intrinsic_Type_Spec, 'COMPLEX'),
     ('character', Intrinsic_Type_Spec, 'CHARACTER'),
     ('logical', Intrinsic_Type_Spec, 'LOGICAL'),
     ('type(foo)', Declaration_Type_Spec, 'TYPE(foo)'),
     ('class(bar)', Declaration_Type_Spec, 'CLASS(bar)'),
     ('class(*)', Declaration_Type_Spec, 'CLASS(*)'),
     ('elemental', Prefix_Spec, 'ELEMENTAL'),
     ('impure', Prefix_Spec, 'IMPURE'),
     ('module', Prefix_Spec, 'MODULE'),
     ('pure', Prefix_Spec, 'PURE'),
     ('recursive', Prefix_Spec, 'RECURSIVE')])
def test_Prefix_Spec(procedure_prefix_input, expected_class,
                     expected_string):  # R1226
    unit_under_test = Prefix_Spec
    result = unit_under_test(procedure_prefix_input)
    assert isinstance(result, expected_class), repr(result)
    assert str(result) == expected_string


def test_Suffix():  # R1229

    cls = Suffix

    obj = cls('bind(c)')
    assert isinstance(obj, Language_Binding_Spec), repr(obj)
    assert str(obj) == 'BIND(C)'
    assert repr(obj) == "Language_Binding_Spec(None)"

    obj = cls('result(a)')
    assert isinstance(obj, Suffix), repr(obj)
    assert str(obj) == 'RESULT(a)'

    obj = cls('bind(c) result(a)')
    assert isinstance(obj, Suffix), repr(obj)
    assert str(obj) == 'RESULT(a) BIND(C)'

    obj = cls('result(a) bind(c)')
    assert isinstance(obj, Suffix), repr(obj)
    assert str(obj) == 'RESULT(a) BIND(C)'


def test_End_Function_Stmt():  # R1230
    cls = End_Function_Stmt
    obj = cls('end')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'END FUNCTION'

    obj = cls('endfunction')
    assert str(obj) == 'END FUNCTION'

    obj = cls('endfunction foo')
    assert str(obj) == 'END FUNCTION foo'


def test_Subroutine_Subprogram():  # R1231

    reader = get_reader('''\
    subroutine foo
    end subroutine foo''')
    cls = Subroutine_Subprogram
    obj = cls(reader)
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SUBROUTINE foo\nEND SUBROUTINE foo'
    assert (repr(obj) == "Subroutine_Subprogram(Subroutine_Stmt(None, "
            "Name('foo'), None, None), End_Subroutine_Stmt('SUBROUTINE', "
            "Name('foo')))")

    reader = get_reader('''\
    subroutine foo
    integer a
    end subroutine foo''')
    cls = Subroutine_Subprogram
    obj = cls(reader)
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SUBROUTINE foo\n  INTEGER :: a\nEND SUBROUTINE foo'


def test_Subroutine_Stmt():  # R1232

    cls = Subroutine_Stmt
    obj = cls('subroutine foo')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SUBROUTINE foo'
    assert repr(obj) == "Subroutine_Stmt(None, Name('foo'), None, None)"

    obj = cls('pure subroutine foo')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PURE SUBROUTINE foo'

    obj = cls('pure subroutine foo(a,b)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'PURE SUBROUTINE foo(a, b)'

    obj = cls('subroutine foo() bind(c)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SUBROUTINE foo BIND(C)'

    obj = cls('subroutine foo(a)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SUBROUTINE foo(a)'

    obj = cls('subroutine foo(a, b)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SUBROUTINE foo(a, b)'

    obj = cls('subroutine foo(a,*)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SUBROUTINE foo(a, *)'

    obj = cls('subroutine foo(*)')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SUBROUTINE foo(*)'


def test_Dummy_Arg():  # R1233
    cls = Dummy_Arg
    obj = cls('a')
    assert isinstance(obj, Name), repr(obj)
    assert str(obj) == 'a'
    obj = cls('*')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == '*'


def test_End_Subroutine_Stmt():  # R1234

    cls = End_Subroutine_Stmt
    obj = cls('end subroutine foo')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'END SUBROUTINE foo'
    assert repr(obj) == "End_Subroutine_Stmt('SUBROUTINE', Name('foo'))"

    obj = cls('end')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'END SUBROUTINE'

    obj = cls('endsubroutine')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'END SUBROUTINE'


def test_Entry_Stmt():  # R1235

    cls = Entry_Stmt
    obj = cls('entry a')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'ENTRY a()'

    obj = cls('entry a()')
    assert str(obj) == 'ENTRY a()'

    obj = cls('entry a(b, c)')
    assert str(obj) == 'ENTRY a(b, c)'

    obj = cls('entry a(b, c) bind(c)')
    assert str(obj) == 'ENTRY a(b, c) BIND(C)'


def test_Return_Stmt():  # R1236

    cls = Return_Stmt
    obj = cls('return')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'RETURN'
    assert repr(obj) == 'Return_Stmt(None)'


def test_Contains():  # R1237

    cls = Contains_Stmt
    obj = cls('Contains')
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'CONTAINS'
    assert repr(obj) == "Contains_Stmt('CONTAINS')"


def test_multi_unit():
    ''' Check what happens when we have more than one program/routine
    in a file '''
    cls = Program
    reader = get_reader('''\
      program foo
        integer :: my_int
        my_int = my_func()
        write(*,*) my_int
      end program
      function my_func()
        integer :: my_func
        my_func = 2
      end function''')
    obj = cls(reader)
    assert type(obj) == Program
    output = str(obj)
    assert "PROGRAM foo" in output
    assert "FUNCTION my_func()" in output
    assert output.endswith("END FUNCTION")


if 0:
    NOF_NEEDED_TESTS = 0
    NOF_NEEDED_MATCH = 0
    TOTAL_NEEDS = 0
    TOTAL_CLASSES = 0
    for NAME in dir():
        OBJ = eval(NAME)
        if not isinstance(OBJ, ClassType):
            continue
        if not issubclass(OBJ, Base):
            continue
        CLSNAME = OBJ.__name__
        if CLSNAME.endswith('Base'):
            continue
        TOTAL_CLASSES += 1
        SUBCLASS_NAMES = OBJ.__dict__.get('subclass_names', None)
        USE_NAMES = OBJ.__dict__.get('use_names', None)
        if not USE_NAMES:
            continue
        MATCH = OBJ.__dict__.get('match', None)
        try:
            TEST_CLS = eval('test_{0}'.format(CLSNAME))
        except NameError:
            TEST_CLS = None
        TOTAL_NEEDS += 1
        if MATCH is None:
            if TEST_CLS is None:
                print('Needs tests:', CLSNAME)
                print('Needs match implementation:', CLSNAME)
                NOF_NEEDED_TESTS += 1
                NOF_NEEDED_MATCH += 1
            else:
                print('Needs match implementation:', CLSNAME)
                NOF_NEEDED_MATCH += 1
        else:
            if TEST_CLS is None:
                print('Needs tests:', CLSNAME)
                NOF_NEEDED_TESTS += 1
        continue
    print('-----')
    print('Nof match implementation needs:', NOF_NEEDED_MATCH,
          'out of', TOTAL_NEEDS)
    print('Nof tests needs:', NOF_NEEDED_TESTS, 'out of', TOTAL_NEEDS)
    print('Total number of classes:', TOTAL_CLASSES)
    print('-----')
