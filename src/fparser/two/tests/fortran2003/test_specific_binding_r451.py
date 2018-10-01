# Copyright (c) 2018 Science and Technology Facilities Council

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

'''
Test Fortran 2003 rule R451 : This file tests the support for specific
binding and passed-object dummy argument within the type-bound procedure
part of a derived type.
'''

import pytest
from fparser.api import get_reader
from fparser.two.utils import FortranSyntaxError, NoMatchError
from fparser.two.Fortran2003 import Specific_Binding, Binding_PASS_Arg_Name, \
    Program

SOURCE = '''\
module test_mod

  implicit none
  private

  ! Define test type
  type, public, extends(abstract_type) :: implementation_type
    private

  contains

    procedure :: internal_method_1
    procedure internal_method_2
    procedure, public :: submethod_1
    procedure, public, pass(self) :: abstract_method => implementation_method

  end type implementation_type

end module test_mod'''


def test_specific_binding(f2003_create):
    ''' Test that individual Specific_Binding statements are
    parsed correctly. '''
    testcls = Specific_Binding

    lines = SOURCE.split('\n')

    obj = testcls('procedure :: sub_gen => sub_spec')
    assert isinstance(obj, testcls), repr(obj)
    assert str(obj) == "PROCEDURE :: sub_gen => sub_spec"

    obj = testcls('procedure, pass :: length => point_length')
    assert isinstance(obj, testcls), repr(obj)
    assert str(obj) == "PROCEDURE, PASS :: length => point_length"

    obj = testcls(lines[11].strip())
    assert isinstance(obj, testcls), repr(obj)
    assert str(obj) == "PROCEDURE :: internal_method_1"

    obj = testcls(lines[12].strip())
    assert isinstance(obj, testcls), repr(obj)
    assert str(obj) == "PROCEDURE internal_method_2"

    obj = testcls(lines[13].strip())
    assert isinstance(obj, testcls), repr(obj)
    assert str(obj) == "PROCEDURE, PUBLIC :: submethod_1"


def test_binding_pass_arg_name(f2003_create):
    ''' Test that Binding_PASS_Arg_Name is parsed correctly. '''

    testcls = Binding_PASS_Arg_Name
    parsecls = Specific_Binding

    aobj = testcls("pass(name)")
    assert isinstance(aobj, testcls), repr(aobj)
    assert repr(aobj) == ("Binding_PASS_Arg_Name('PASS', Name('name'))")

    line = ("procedure(analytic_function), deferred, pass(self) :: "
            "function_name")
    aobj = parsecls(line).items[1].items[1]
    assert isinstance(aobj, testcls), repr(aobj)
    assert repr(aobj) == "Binding_PASS_Arg_Name('PASS', Name('self'))"

    line = SOURCE.split('\n')[14].strip()
    obj = parsecls(line)
    assert isinstance(obj, parsecls), repr(obj)
    assert (str(obj) == "PROCEDURE, PUBLIC, PASS(self) :: "
            "abstract_method => implementation_method")
    assert repr(obj) == (
        "Specific_Binding(None, "
        "Binding_Attr_List(',', (Access_Spec('PUBLIC'), "
        "Binding_PASS_Arg_Name('PASS', Name('self')))), "
        "'::', Name('abstract_method'), Name('implementation_method'))")


def test_specific_binding_error(f2003_create):
    ''' Test that parsing invalid Fortran syntax for
    Specific_Binding statements raises an appropriate error. '''
    testcls = Specific_Binding

    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls('procedure populate_entity => populate_cube')
    assert ("Specific_Binding: 'procedure populate_entity => "
            "populate_cube'" in str(excinfo))

    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls('procedure :: populate_entity =>')
    assert ("Specific_Binding: 'procedure :: populate_entity =>"
            in str(excinfo))

    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls('procedure, :: sub_gen => sub_spec')
    assert ("Specific_Binding: 'procedure, :: sub_gen => sub_spec'"
            in str(excinfo))

    line = SOURCE.split('\n')[13].strip()
    line = line.replace(" :: ", " ")
    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls(line)
    assert ("Specific_Binding: 'procedure, public submethod_1'"
            in str(excinfo))


def test_specific_binding_fserror(f2003_create):
    ''' Test that an error in Specific_Binding within a program
    unit (here module) is raised as a FortranSyntaxError. '''

    code = SOURCE.replace("procedure :: internal_method_1",
                          "proced :: internal_method_1")
    reader = get_reader(code)
    with pytest.raises(FortranSyntaxError) as excinfo:
        dummy_ = Program(reader)
    assert ("at line 12\n>>>    proced :: internal_method_1\n"
            in str(excinfo.value))

    code = SOURCE.replace("procedure, public :: submethod_1",
                          "procedure, public ::")
    reader = get_reader(code)
    with pytest.raises(FortranSyntaxError) as excinfo:
        dummy_ = Program(reader)
    assert ("at line 14\n>>>    procedure, public ::\n"
            in str(excinfo.value))

    code = SOURCE.replace("procedure, public, pass(self) :: "
                          "abstract_method => implementation_method",
                          "procedure abstract_method => implementation_method")
    reader = get_reader(code)
    with pytest.raises(FortranSyntaxError) as excinfo:
        dummy_ = Program(reader)
    assert ("at line 15\n>>>    procedure abstract_method =>"
            in str(excinfo.value))
