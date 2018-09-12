# Copyright (c) 2017-2018 Science and Technology Facilities Council

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

##############################################################################
# Modified I. Kavcic, UK Met Office
##############################################################################

'''
Test Fortran 2003 rule R911 : This file tests the support for various
forms of WRITE statements.
'''

import pytest
from fparser.api import get_reader
from fparser.two.utils import FortranSyntaxError, NoMatchError
from fparser.two.Fortran2003 import Write_Stmt, Io_Control_Spec_List, \
    Output_Item_List, Program


def test_write_stmt(f2003_create):
    ''' Tests for various forms of WRITE statement. '''
    tcls = Write_Stmt
    obj = tcls('write (123)"hey"')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'WRITE(123) "hey"'
    assert repr(obj) == (
        "Write_Stmt(Io_Control_Spec_List(',', "
        "(Io_Control_Spec(None, Int_Literal_Constant('123', None)),)), "
        "Char_Literal_Constant('\"hey\"', None))")

    obj = tcls('WRITE (*,"(I3)") my_int')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'WRITE(*, FMT = "(I3)") my_int'
    assert repr(obj) == (
        "Write_Stmt(Io_Control_Spec_List(',', "
        "(Io_Control_Spec(None, Io_Unit('*')), Io_Control_Spec('FMT', "
        "Char_Literal_Constant('\"(I3)\"', None)))), Name('my_int'))")

    obj = tcls('WRITE (*,namtest)')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'WRITE(*, namtest)'
    assert repr(obj) == (
        "Write_Stmt(Io_Control_Spec_List(',', "
        "(Io_Control_Spec(None, Io_Unit('*')), Io_Control_Spec(None, "
        "Name('namtest')))), None)")

    # Test when format specifier contains an '=' character
    iolist = Io_Control_Spec_List("*,'(5X,\"q_mesh =\",4F12.8)'")

    assert isinstance(iolist, Io_Control_Spec_List)
    obj = tcls("WRITE(*,'(5X,\"q_mesh =\",1F12.8)') 1.d0")
    assert isinstance(obj, tcls)
    assert repr(obj) == (
        "Write_Stmt(Io_Control_Spec_List(\',\', "
        "(Io_Control_Spec(None, Io_Unit(\'*\')), "
        "Io_Control_Spec(None, "
        "Char_Literal_Constant(\'\\\'(5X,\"q_mesh =\",1F12.8)\\\'\', "
        "None)))), Real_Literal_Constant(\'1.D0\', None))")

    obj = tcls("WRITE(*,FMT='(5X,\"q_mesh =\",1F12.8)') 1.d0")
    assert isinstance(obj, tcls)
    assert repr(obj) == (
        "Write_Stmt(Io_Control_Spec_List(\',\', "
        "(Io_Control_Spec(None, Io_Unit(\'*\')), "
        "Io_Control_Spec(\'FMT\', "
        "Char_Literal_Constant(\'\\\'(5X,\"q_mesh =\",1F12.8)\\\'\', "
        "None)))), Real_Literal_Constant(\'1.D0\', None))")


def test_write_output_item_list(f2003_create):
    ''' Test that the Output_Item_List part of Write_Stmt can be
    parsed correctly. '''
    tcls = Output_Item_List

    obj = tcls("\" = \", length, width")
    assert isinstance(obj, tcls)
    assert str(obj) == '" = ", length, width'
    assert repr(obj) == (
        "Output_Item_List(\',\', (Char_Literal_Constant(\'\" = \"\', None), "
        "Name(\'length\'), Name(\'width\')))")

    with pytest.raises(NoMatchError) as excinfo:
        _ = tcls("'loop indices (o, i): ', , outer, inner")
    assert ("NoMatchError: Output_Item_List: "
            "''loop indices (o, i): ', , outer, inner'") in str(excinfo)


def test_raise_write_error(f2003_create):
    ''' Test that an error in Write_Stmt within a program unit
    is raised as a FortranSyntaxError. '''
    source = '''\
program test

  implicit none

  integer, parameter :: scratch_unit = 10
  integer :: outer, inner

  write( scratch_unit, '(A,2I3)' ) 'loop indices (o, i): ', &
                                   , outer, inner
end program test'''

    reader = get_reader(source)
    with pytest.raises(FortranSyntaxError) as excinfo:
        dummy_ = Program(reader)
    assert ("at line 9\n"
            ">>>                                   , outer, inner"
            in str(excinfo.value))
