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
Test Fortran 2003 rule R449 : This file tests the support for binding private
statement within the type-bound procedure part of a derived type.
'''

import pytest
from fparser.two.utils import NoMatchError
from fparser.two.Fortran2003 import Binding_Private_Stmt

SOURCE = '''\
module test_mod

  implicit none
  private

  ! Define test type
  type, public, extends(abstract_type) :: implementation_type
    private

  contains

    procedure :: internal_method_1
    procedure, public :: submethod_1

  end type implementation_type

end module test_mod'''


def test_binding_private_stmt(f2003_create):
    ''' Test that a Binding_Private_Stmt is parsed correctly. '''

    lines = SOURCE.split('\n')

    obj = Binding_Private_Stmt(lines[7].strip())
    assert isinstance(obj, Binding_Private_Stmt), repr(obj)
    assert str(obj) == "PRIVATE"
    assert repr(obj) == "Binding_Private_Stmt('PRIVATE')"


def test_error_binding_private_stmt(f2003_create):
    ''' Test that parsing invalid Fortran syntax for
    Binding_Private_Stmt statement raises an appropriate error. '''

    line = SOURCE.split('\n')[7].strip()
    line = line.replace("private", "privatte")
    with pytest.raises(NoMatchError) as excinfo:
        _ = Binding_Private_Stmt(line)
    assert ("Binding_Private_Stmt: 'privatte'"
            in str(excinfo))
