# Copyright (c) 2019 Science and Technology Facilities Council

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

'''Test Fortran Include Statement: This file tests that we succesfully
parse an include statement. Whilst include is not part of the standard
rules (the include should include code as the code is being parsed)
there are cases where users might like to keep the include statement
in the Fortran parse tree and output it again.

'''

import pytest
from fparser.api import get_reader
from fparser.two.Fortran2003 import Include_Stmt
from fparser.two.utils import NoMatchError


def test_include_stmt(f2003_create):
    '''Check that a basic Cray-pointer statement is parsed
    correctly. Input separately as a string and as a reader object

    '''
    def check_include(reader):
        '''Internal helper function to avoid code replication.'''
        ast = Include_Stmt(reader)
        assert "INCLUDE 'my-non-existant-file.inc'" in str(ast)
        assert repr(ast) == ("****")

    line = "include 'my-non-existant-file.inc'"
    check_include(line)
    reader = get_reader(line)
    check_include(reader)


def test_spaces(f2003_create):
    '''Check that spaces are allowed.'''
    line = " include 'my-non-existant-file.inc' "
    ast = include_Stmt(line)
    assert "POINTER(a, b)" in str(ast)


def test_no_space(f2003_create):
    '''Check that spaces are allowed.'''
    line = "include'my-non-existant-file.inc'"
    ast = Include_Stmt(line)
    assert "POINTER(a, b)" in str(ast)


def test_case(f2003_create):
    '''Check that different case is allowed.'''
    line = "InClUdE 'my-non-existant-file.inc'"
    ast = Include_Stmt(line)
    assert "POINTER(a, b)" in str(ast)


def test_double_quotes(f2003_create):
    '''Check that different case is allowed.'''
    line = 'include "my-non-existant-file.inc"'
    ast = Include_Stmt(line)
    assert "POINTER(a, b)" in str(ast)


def test_errors(f2003_create):
    '''Check that syntax errors produce a NoMatchError exception.'''
    for line in ["", "  "]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Include_Stmt(line)
        assert "Include_Stmt: '{0}'".format(line) in str(excinfo)
