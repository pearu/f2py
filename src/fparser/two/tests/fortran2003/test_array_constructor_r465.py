# Copyright (c) 2020 Science and Technology Facilities Council.
#
# All rights reserved.
#
# Modifications made as part of the fparser project are distributed
# under the following license:
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
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

'''Test Fortran 2003 rule R465: this file tests the support for the
various forms of array constructor.

'''

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two.utils import walk
from fparser.two import Fortran2003


@pytest.mark.usefixtures("f2003_create")
def test_basic_array_constructor():
    ''' Test parsing of basic array constructor. '''
    fcode = "array = (/ 1, 2, 3/)"
    reader = FortranStringReader(fcode)
    ast = Fortran2003.Assignment_Stmt(reader)
    assert isinstance(ast, Fortran2003.Assignment_Stmt)
    assert isinstance(ast.children[2], Fortran2003.Array_Constructor)
    assert isinstance(ast.children[2].children[1], Fortran2003.Ac_Value_List)
    assert "array = (/1, 2, 3/)" in str(ast)


@pytest.mark.usefixtures("f2003_create")
def test_zero_size_array_constructor():
    ''' Test that we can parse a valid, zero-size array constructor (R466). '''
    fcode = "array = (/ integer :: /)"
    reader = FortranStringReader(fcode)
    ast = Fortran2003.Assignment_Stmt(reader)
    assert isinstance(ast, Fortran2003.Assignment_Stmt)
    assert isinstance(ast.children[2], Fortran2003.Array_Constructor)
    assert isinstance(ast.children[2].children[1], Fortran2003.Ac_Spec)
    assert isinstance(ast.children[2].children[1].children[0],
                      Fortran2003.Intrinsic_Type_Spec)
    assert "array = (/INTEGER ::/)" in str(ast)


@pytest.mark.usefixtures("f2003_create")
def test_constructor_char_len():
    ''' Test with an array constructor that specifies a length type
        parameter and uses square brackets. '''
    fcode = "array = [ CHARACTER(LEN=7) :: 'Takata', 'Tanaka', 'Hayashi' ]"
    reader = FortranStringReader(fcode)
    ast = Fortran2003.Assignment_Stmt(reader)
    assert isinstance(ast, Fortran2003.Assignment_Stmt)
    assert isinstance(ast.children[2], Fortran2003.Array_Constructor)
    assert isinstance(ast.children[2].children[1], Fortran2003.Ac_Spec)
    assert isinstance(ast.children[2].children[1].children[0],
                      Fortran2003.Intrinsic_Type_Spec)
    assert ("array = [CHARACTER(LEN = 7) :: 'Takata', 'Tanaka', 'Hayashi']"
            in str(ast))


@pytest.mark.usefixtures("f2003_create")
def test_implicit_loop_constructor():
    ''' Test array constructor with implicit loop containing an intrinsic
    call. '''
    fcode = "WHERE((/(JBODY,JBODY=1,SIZE(ARR1(:)))/)/=1) ARR1(:)=1.0"
    reader = FortranStringReader(fcode)
    ast = Fortran2003.Where_Stmt(reader)
    assert isinstance(ast, Fortran2003.Where_Stmt)
    constructor = walk(ast, Fortran2003.Array_Constructor)[0]
    do_control = constructor.children[1].children[0].children[1]
    assert isinstance(do_control, Fortran2003.Ac_Implied_Do_Control)
    assert "(JBODY, JBODY = 1, SIZE(ARR1(:)))" in str(ast)


@pytest.mark.xfail(reason="#255 - Structure_Constructor_2 ends up matching "
                   "key=var pattern.")
@pytest.mark.usefixtures("f2003_create")
def test_implicit_loop_constructor_no_parentheses():
    ''' Test that the parser does not match an implicit loop if the
    surrounding parentheses are missing (R470). '''
    fcode = "(/ival, ival=1, nval, istep/)"
    reader = FortranStringReader(fcode)
    ast = Fortran2003.Array_Constructor(reader)
    assert ast is None
