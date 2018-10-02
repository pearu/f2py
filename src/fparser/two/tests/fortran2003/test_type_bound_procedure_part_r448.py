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
Test Fortran 2003 rule R448 : This file tests the support for
type-bound procedure part of a derived type.
'''

import pytest
from fparser.api import get_reader
from fparser.two.utils import NoMatchError
from fparser.two.Fortran2003 import Type_Bound_Procedure_Part


SOURCE = '''\
contains
  procedure, non_overridable :: view_method
  procedure, public :: method_1
  generic, public :: method => method_1
  final method_destructor'''

EXPECTED = '''\
CONTAINS
PROCEDURE, NON_OVERRIDABLE :: view_method
PROCEDURE, PUBLIC :: method_1
GENERIC, PUBLIC :: method => method_1
FINAL :: method_destructor'''


def test_type_bound_procedure_part(f2003_create):
    ''' Test that Type_Bound_Procedure_Part code block
    is parsed correctly. '''

    # Define test class
    testcls = Type_Bound_Procedure_Part

    # Test type-bound procedure parts
    expected = EXPECTED.strip().split('\n')
    reader = get_reader(SOURCE)
    obj = testcls(reader)

    result = str(obj).strip().split('\n')
    assert result == expected
    assert isinstance(obj, testcls), repr(obj)


def test_type_bound_procedure_part_error(f2003_create):
    # pylint: disable=invalid-name
    ''' Test that parsing invalid Fortran syntax for
    Type_Bound_Procedure_Part code block raises an appropriate error. '''
    testcls = Type_Bound_Procedure_Part

    code = SOURCE.replace("contains",
                          "contai")
    reader = get_reader(code)
    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls(reader)
    assert "NoMatchError: at line 1" in str(excinfo)
