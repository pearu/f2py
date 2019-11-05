# Copyright (c) 2018-2019 Science and Technology Facilities Council

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

"""
Test support for preprocessor directives:

This file tests the parsing of common preprocessor directives. These
are not part of the Fortran standard, but are often included in existing
codebases, so it can be advantageous to safely parse them as black-box
nodes. This behaviour should be guarded by a flag to ensure errors for
tools that expect more rigorous semantic checks.
"""

import pytest
from fparser.api import get_reader
from fparser.two.Fortran2003 import Program, Include_Stmt, Define_Stmt
from fparser.two.utils import NoMatchError, InternalError


def test_subroutine_external_include(f2003_create):
    """
    Check that a basic, unprocessed include statement is parsed
    correctly outside of a subroutine call.
    """
    code = """
#include "foo.h"

subroutine bar()
    print *, "Hello foo!"
end subroutine bar
"""
    reader = get_reader(code)
    ast = Program(reader)
    assert "INCLUDE 'foo.h'" in str(ast)
    assert isinstance(ast.content[0], Include_Stmt)


def test_subroutine_external_define(f2003_create):
    """
    Check that a basic, unprocessed ``#ifdef`` statements.
    """
    code = """
#define FOO "Hello Foo"

subroutine bar()
    print *, FOO
end subroutine bar
"""
    reader = get_reader(code)
    ast = Program(reader)
    assert 'DEFINE FOO "Hello Foo"' in str(ast)
    assert isinstance(ast.content[0], Define_Stmt)


def test_subroutine_external_ifdef(f2003_create):
    """
    Check that a basic, unprocessed ``#ifdef`` statements.
    """
    code = """
#ifdef BAZ
#define FOO "Hello Foo"
#else
#define FOO "Sorry, not FOO!"
#endif

subroutine bar()
    print *, FOO
end subroutine bar
"""

    reader = get_reader(code)
    ast = Program(reader)
    #TODO: assert
