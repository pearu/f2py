# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017-2018 Science and Technology Facilities Council
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
##############################################################################
# Modified M. Hambley, UK Met Office
# Modified I. Kavcic, UK Met Office
##############################################################################
'''
Test Fortran 2003 rule R453 : This file tests the support for allowed binding
attributes for a specific type-bound procedure binding within a derived type.
'''

import pytest
from fparser.api import get_reader
from fparser.two.utils import FortranSyntaxError, NoMatchError
from fparser.two.Fortran2003 import Binding_Attr, Binding_Attr_List, \
    Access_Spec, Specific_Binding, Program, Derived_Type_Def


def test_pass_binding_attr(f2003_create):
    '''
    Test that the parser understands PASS binding atribute.
    '''

    # Define source code to test
    source = '''\
type :: velocity_vector
  real :: u, v, w
  contains
    procedure, pass :: speed => calculate_speed
end type velocity_vector
'''

    # Define test classes for individual statements and code block
    testcls = Binding_Attr
    testunitcls = Derived_Type_Def

    # Test individual PASS attribute
    line = source.split('\n')[3].strip()
    obj = Specific_Binding(line).items[1]
    assert isinstance(obj, testcls), repr(obj)
    assert str(obj) == 'PASS'
    assert repr(obj) == "Binding_Attr('PASS')"

    line = line.replace('pass', 'past')
    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls(line)
    assert "Binding_Attr: 'procedure, past" in str(excinfo)

    # Test the entire source
    expected = '''\
TYPE :: velocity_vector
  REAL :: u, v, w
  CONTAINS
  PROCEDURE, PASS :: speed => calculate_speed
END TYPE velocity_vector
'''.strip().split('\n')

    reader = get_reader(source, ignore_comments=False)
    test_unit = testunitcls(reader)

    result = str(test_unit).strip().split('\n')
    assert result == expected


def test_nopass_binding_attr(f2003_create):
    '''
    Test that the parser understands NOPASS binding atribute.
    '''

    # Define source code to test
    source = '''\
type, public, extends(kernel_type) :: buoyancy_kernel_type
  private
  integer :: iterates_over = CELLS
  contains
    procedure, nopass :: initial_buoyancy_code
end type
'''

    # Define test classes for individual statements and code block
    testcls = Binding_Attr
    testunitcls = Derived_Type_Def

    # Test individual NOPASS attribute
    line = source.split('\n')[4].strip()
    obj = Specific_Binding(line).items[1]
    assert isinstance(obj, testcls), repr(obj)
    assert str(obj) == 'NOPASS'
    assert repr(obj) == "Binding_Attr('NOPASS')"

    line = line.replace('nopass', 'nopepas')
    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls(line)
    assert "Binding_Attr: 'procedure, nopepas :: " in str(excinfo)

    # Test the entire source
    expected = '''\
TYPE, PUBLIC, EXTENDS(kernel_type) :: buoyancy_kernel_type
  PRIVATE
  INTEGER :: iterates_over = CELLS
  CONTAINS
  PROCEDURE, NOPASS :: initial_buoyancy_code
END TYPE buoyancy_kernel_type
'''.strip().split('\n')

    reader = get_reader(source, ignore_comments=False)
    test_unit = testunitcls(reader)

    result = str(test_unit).strip().split('\n')
    assert result == expected


def test_nonoverridable_binding_attr(f2003_create):
    # pylint: disable=invalid-name
    '''
    Test that the parser understands NON_OVERRIDABLE binding atribute.
    '''

    # Define source code to test
    source = '''! Employee definition
module employee_mod

  implicit none
  private

  type, public :: employee_type
    private
    character(len=50), public :: name
    integer :: age
  contains
    procedure, non_overridable :: print_info => print_per_info
  end type employee_type

end module employee_mod
'''

    # Define test classes for individual statements and code block
    testcls = Binding_Attr
    testunitcls = Program

    # Test individual NON_OVERRIDABLE attribute
    line = source.split('\n')[11].strip()
    obj = Specific_Binding(line).items[1]
    assert isinstance(obj, testcls), repr(obj)
    assert str(obj) == 'NON_OVERRIDABLE'
    assert repr(obj) == "Binding_Attr('NON_OVERRIDABLE')"

    line = line.replace('non_overridable', 'nonoverridable')
    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls(line)
    assert "Binding_Attr: 'procedure, nonoverridable ::" in str(excinfo)

    # Test the entire source
    expected = '''! Employee definition
MODULE employee_mod

  IMPLICIT NONE
  PRIVATE

  TYPE, PUBLIC :: employee_type
    PRIVATE
    CHARACTER(LEN = 50), PUBLIC :: name
    INTEGER :: age
    CONTAINS
    PROCEDURE, NON_OVERRIDABLE :: print_info => print_per_info
  END TYPE employee_type

END MODULE employee_mod
'''.strip().split('\n')

    reader = get_reader(source, ignore_comments=False)
    test_unit = testunitcls(reader)

    result = str(test_unit).strip().split('\n')
    assert result == expected

    code = source.replace("procedure, non_overridable ::",
                          "procedure, non_overridabl ::")
    reader = get_reader(code)
    with pytest.raises(FortranSyntaxError) as excinfo:
        dummy_ = testunitcls(reader)
    assert ("at line 12\n>>>    procedure, non_overridabl ::"
            in str(excinfo.value))


def test_deferred_binding_attr(f2003_create):
    '''
    Test that the parser understands DEFFERED binding atribute.
    '''

    # Define source code to test
    source = '''! Abstract type
module abstract_test

  implicit none
  private

  type, abstract, public :: test_type
  contains
    procedure(method_interface), deferred :: method
  end type test_type
  ! A comment at the end

end module abstract_test
'''

    # Define test classes for individual statements and code block
    testcls = Binding_Attr
    testunitcls = Program

    # Test individual DEFERRED attribute
    line = source.split('\n')[8].strip()
    obj = Specific_Binding(line).items[1]
    assert isinstance(obj, testcls), repr(obj)
    assert str(obj) == 'DEFERRED'
    assert repr(obj) == "Binding_Attr('DEFERRED')"

    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls('deferted')
    assert "Binding_Attr: 'deferted'" in str(excinfo)

    # Test the entire source
    expected = '''! Abstract type
MODULE abstract_test

  IMPLICIT NONE
  PRIVATE

  TYPE, ABSTRACT, PUBLIC :: test_type
    CONTAINS
    PROCEDURE(method_interface), DEFERRED :: method
  END TYPE test_type
  ! A comment at the end

END MODULE abstract_test
'''.strip().split('\n')

    reader = get_reader(source, ignore_comments=False)
    test_unit = testunitcls(reader)

    result = str(test_unit).strip().split('\n')
    assert result == expected

    code = source.replace("procedure(method_interface), deferred :: method",
                          "procedure(method_interface), deferre :: method")
    reader = get_reader(code)
    with pytest.raises(FortranSyntaxError) as excinfo:
        dummy_ = testunitcls(reader)
    assert ("at line 9\n>>>    procedure(method_interface), deferre ::"
            in str(excinfo.value))


def test_accspec_binding_attr(f2003_create):
    '''
    Test that the parser understands Access_Spec binding atributes
    (PUBLIC and PRIVATE).
    '''

    # Define test classes
    testcls = Access_Spec
    attrcls = Binding_Attr_List

    # Test PUBLIC attribute
    line = "procedure, public, pass(self) :: print_info => print_per_info"

    bind_attr = Specific_Binding(line).items[1]
    access_attr = bind_attr.items[0]
    assert isinstance(bind_attr, attrcls), repr(bind_attr)
    assert isinstance(access_attr, testcls), repr(access_attr)
    assert str(access_attr) == 'PUBLIC'
    assert repr(access_attr) == "Access_Spec('PUBLIC')"

    line = line.replace('public', 'publi')
    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls(line)
    assert "Access_Spec: 'procedure, publi, pass(self)" in str(excinfo)

    # Test PRIVATE attribute
    line = "procedure, private :: test_private"

    access_attr = Specific_Binding(line).items[1]
    assert isinstance(access_attr, testcls), repr(access_attr)
    assert str(access_attr) == 'PRIVATE'
    assert repr(access_attr) == "Access_Spec('PRIVATE')"

    line = line.replace('private', 'priate')
    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls(line)
    assert "Access_Spec: 'procedure, priate ::" in str(excinfo)
