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
Test Fortran 2003 rule R452 : This file tests the support for generic
binding within a type-bound procedure part of a derived type.
'''

import pytest
from fparser.api import get_reader
from fparser.two.utils import FortranSyntaxError, NoMatchError
from fparser.two.Fortran2003 import Generic_Binding, Access_Spec, \
    Binding_Name_List, Program

SOURCE = '''\
module test_mod

  implicit none
  private

  ! Define test type
  type, public, extends(abstract_type) :: implementation_type
    private

  contains

    procedure, public :: method_1
    procedure, public :: method_2
    procedure, public :: method_3
    generic           :: method => method_1, method_2, method_3

  end type implementation_type

end module test_mod'''

EXPECTED = '''\
MODULE test_mod

  IMPLICIT NONE
  PRIVATE

  ! Define test type
  TYPE, PUBLIC, EXTENDS(abstract_type) :: implementation_type
    PRIVATE

    CONTAINS

    PROCEDURE, PUBLIC :: method_1
    PROCEDURE, PUBLIC :: method_2
    PROCEDURE, PUBLIC :: method_3
    GENERIC :: method => method_1, method_2, method_3

  END TYPE implementation_type

END MODULE test_mod'''


def test_generic_binding(f2003_create):
    ''' Test that individual Generic_Binding statements are
    parsed correctly. '''

    # Define test classes for individual statements and code block
    testcls = Generic_Binding
    testunitcls = Program

    # Test individual generic binding
    obj = testcls('generic :: a => b')
    assert isinstance(obj, testcls), repr(obj)
    assert str(obj) == 'GENERIC :: a => b'
    assert repr(obj) == "Generic_Binding(None, Name('a'), Name('b'))"

    obj = testcls('generic, private :: read(formatted) => b,c')
    assert isinstance(obj, testcls), repr(obj)
    assert str(obj) == 'GENERIC, PRIVATE :: READ(FORMATTED) => b, c'
    assert repr(obj) == ("Generic_Binding(Access_Spec('PRIVATE'), "
                         "Dtio_Generic_Spec('READ(FORMATTED)'), "
                         "Binding_Name_List(',', (Name('b'), Name('c'))))")
    acspobj = obj.items[0]
    assert isinstance(acspobj, Access_Spec), repr(acspobj)
    bnlobj = obj.items[2]
    assert isinstance(bnlobj, Binding_Name_List), repr(bnlobj)

    # Test the entire source
    expected = EXPECTED.strip().split('\n')
    reader = get_reader(SOURCE, ignore_comments=False)
    test_unit = testunitcls(reader)

    result = str(test_unit).strip().split('\n')
    assert result == expected


def test_generic_binding_error(f2003_create):
    ''' Test that parsing invalid Fortran syntax for
    Generic_Binding statements raises an appropriate error. '''
    testcls = Generic_Binding

    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls('generi :: a')
    assert "Generic_Binding: 'generi :: a'" in str(excinfo)

    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls('generic :: b,')
    assert "Generic_Binding: 'generic :: b," in str(excinfo)

    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls('generic a')
    assert "Generic_Binding: 'generic a'" in str(excinfo)

    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls('generic :: a +> b')
    assert "Generic_Binding: 'generic :: a +> b'" in str(excinfo)

    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls('generic :: a b')
    assert "Generic_Binding: 'generic :: a b'" in str(excinfo)

    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls('generic a => b')
    assert "Generic_Binding: 'generic a => b'" in str(excinfo)

    line = SOURCE.split('\n')[14].strip()
    line = line.replace(" :: ", " : ")
    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls(line)
    assert ("Generic_Binding: 'generic           : method => method_1"
            in str(excinfo))


def test_generic_binding_fserror(f2003_create):
    ''' Test that an error in Generic_Binding within a program
    unit (here module) is raised as a FortranSyntaxError. '''

    code = SOURCE.replace("generic           :: method => method_1",
                          "generic method => method_1")
    reader = get_reader(code)
    with pytest.raises(FortranSyntaxError) as excinfo:
        dummy_ = Program(reader)
    assert ("at line 15\n>>>    generic method => method_1"
            in str(excinfo.value))
