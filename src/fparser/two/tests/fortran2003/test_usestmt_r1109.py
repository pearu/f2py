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

'''Test Fortran 2003 rule R1109 : This file tests the support for the
Use statement.

'''

import pytest
from fparser.api import get_reader
from fparser.two.Fortran2003 import Use_Stmt

# match() use ...


# match() 'use x'
def test_use(f2003_create):
    '''Just get something working to start off with'''
    reader = get_reader("use my_model")
    ast = Use_Stmt(reader)
    assert "USE my_model" in str(ast)


# match() 'use :: x'
def test_use_colons(f2003_create):
    '''Just get something working to start off with'''
    reader = get_reader("use :: my_model")
    ast = Use_Stmt(reader)
    assert "USE :: my_model" in str(ast)


# match() 'use, nature :: x'
def test_use_nature(f2003_create):
    '''Just get something working to start off with'''
    reader = get_reader("use, intrinsic :: my_model")
    ast = Use_Stmt(reader)
    assert "USE, INTRINSIC :: my_model" in str(ast)


# match() 'use x, rename'
def test_use_rename(f2003_create):
    '''Just get something working to start off with'''
    reader = get_reader("use my_model, name=>new_name")
    ast = Use_Stmt(reader)
    assert "USE my_model, name => new_name" in str(ast)


# match() 'use x, only: y'
def test_use_only(f2003_create):
    '''Just get something working to start off with'''
    reader = get_reader("use my_model, only: name")
    ast = Use_Stmt(reader)
    assert "USE my_model, ONLY: name" in str(ast)


# match() 'use x, only:'
def test_use_only_empty(f2003_create):
    '''Just get something working to start off with'''
    reader = get_reader("use my_model, only:")
    ast = Use_Stmt(reader)
    assert "USE my_model, ONLY:" in str(ast)


# match() '  use  ,  nature  ::  x  ,  name=>new_name'
def test_use_spaces_1(f2003_create):
    '''Just get something working to start off with'''
    reader = get_reader("  use  ,  intrinsic  ::  my_model  ,  name=>new_name  ")
    ast = Use_Stmt(reader)
    assert "USE, INTRINSIC :: my_model, name => new_name" in str(ast)


# match() '  use  ,  nature  ::  x  ,  only  :  name'
def test_use_spaces_2(f2003_create):
    '''Just get something working to start off with'''
    reader = get_reader("  use  ,  intrinsic  ::  my_model  ,  only  :  name  ")
    ast = Use_Stmt(reader)
    assert "USE, INTRINSIC :: my_model, ONLY: name" in str(ast)


# match() mixed case
def test_use_mixed_case(f2003_create):
    '''Just get something working to start off with'''
    reader = get_reader("UsE my_model, OnLy: name")
    ast = Use_Stmt(reader)
    assert "USE my_model, ONLY: name" in str(ast)

# match() Syntax errors


def test_syntaxerror(f2003_create):
    '''Test that None is returned if: the line is shorter than 3
    characters, it is miss-spelt, the line is empty after use, there
    is no space after use, nature is missing, there is a missing name
    after nature, there is a missing :: after nature, there is a
    missing name before only_list (*2), and there is a missing : after
    only (*2)

    '''
    for name in ["us", "ust", "use", "usemy_model", "use, ::",
                 "use, intrinsic::", "use, intrinsic my_module", "use,",
                 "use, instrinsic ::,", "use my_model,", "use my_model, only",
                 "use my_model, only ;"]:
        reader = get_reader(name)
        ast = Use_Stmt(reader)
        assert not ast



# match() Internal errors

# tostr() Errors

# misc
