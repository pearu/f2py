# Copyright (c) 2021 Science and Technology Facilities Council
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

''' Module containing tests for the SymbolTables container functionality
 of fparser2. '''

import pytest
from fparser.two.symbol_table import SymbolTables, SymbolTable


def test_construction():
    ''' Check that we can create a SymbolTables instance, add a table to it
    and query it. '''
    tables = SymbolTables()
    assert tables._scope_stack == []
    assert tables._symbol_tables == {}
    with pytest.raises(KeyError) as err:
        tables.lookup("missing")
    assert "missing" in str(err.value)
    # Add a symbol table
    table1 = tables.add("table1")
    assert isinstance(table1, SymbolTable)
    assert tables.lookup("table1") is table1
    # Clear the stored symbol tables
    tables.clear()
    assert tables._scope_stack == []
    assert tables._symbol_tables == {}


def test_scoping_stack():
    ''' Test the functionality related to moving into and out of scoping
    regions. '''
    tables = SymbolTables()
    tables.enter_scope("some_mod")
    outer_table = tables.lookup("some_mod")
    assert tables.current_scope is outer_table
    tables.exit_scope()
    assert tables.current_scope is None
    # Enter scope of existing symbol table
    tables.enter_scope("some_mod")
    assert tables.current_scope is outer_table
    tables.enter_scope("some_func")
    assert tables.current_scope.parent is outer_table
    tables.exit_scope()
    assert tables.current_scope is outer_table
    tables.enter_scope("some_func")
    # The new symbol table should not appear in the top-level symbol tables
    # because it is nested.
    with pytest.raises(KeyError) as err:
        tables.lookup("some_func")
    assert "some_func" in str(err.value)
    assert tables.current_scope.parent is outer_table
    tables.exit_scope()
    tables.exit_scope()
    assert tables.current_scope is None
