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
Test Fortran 2003 rule R450 : This file tests the support for various
procedure bindings for the type-bound procedures within a derived type.
'''

import pytest
from fparser.two.utils import NoMatchError
from fparser.two.Fortran2003 import Proc_Binding_Stmt, \
    Specific_Binding, Generic_Binding, Final_Binding


def test_proc_binding_stmt(f2003_create):
    ''' Test that individual Proc_Binding_Stmt statements are
    parsed correctly. '''

    # Define test class
    testcls = Proc_Binding_Stmt

    # Test individual procedure bindings
    obj = testcls('procedure, pass :: length => point_length')
    assert isinstance(obj, Specific_Binding), repr(obj)
    assert str(obj) == 'PROCEDURE, PASS :: length => point_length'
    assert repr(obj) == (
        "Specific_Binding(None, Binding_Attr('PASS'), '::', "
        "Name('length'), Name('point_length'))")

    obj = testcls('generic, private :: b => c')
    assert isinstance(obj, Generic_Binding), repr(obj)
    assert str(obj) == 'GENERIC, PRIVATE :: b => c'
    assert repr(obj) == (
        "Generic_Binding(Access_Spec('PRIVATE'), Name('b'), Name('c'))")

    obj = testcls('final :: method_destructor')
    assert isinstance(obj, Final_Binding), repr(obj)
    assert str(obj) == 'FINAL :: method_destructor'
    assert repr(obj) == "Final_Binding('FINAL', Name('method_destructor'))"


def test_proc_binding_stmt_error(f2003_create):
    ''' Test that parsing invalid Fortran syntax for
    Proc_Binding_Stmt statements raises an appropriate error. '''
    testcls = Proc_Binding_Stmt

    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls('procedure, pass :: length +> point_length')
    assert ("Proc_Binding_Stmt: 'procedure, pass :: length +>"
            in str(excinfo))

    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls('generic b => ')
    assert "Proc_Binding_Stmt: 'generic b =>" in str(excinfo)

    with pytest.raises(NoMatchError) as excinfo:
        _ = testcls('fina :: a')
    assert "Proc_Binding_Stmt: 'fina :: a'" in str(excinfo)
