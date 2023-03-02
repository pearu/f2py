# Copyright (c) 2022-2023 Science and Technology Facilities Council.

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


import pytest

from fparser.api import get_reader
from fparser.two.Fortran2008 import Block_Construct, Program_Unit
from fparser.two.symbol_table import SymbolTable, SYMBOL_TABLES
from fparser.two.utils import FortranSyntaxError, walk


def test_block(f2008_create):
    """Test that the Block_Construct matches as expected."""
    block = Block_Construct(
        get_reader(
            """\
            block
               integer :: b = 4
               a = 1 + b
            end block
            """
        )
    )

    assert "BLOCK\n  INTEGER :: b = 4\n  a = 1 + b\nEND BLOCK" in str(block)


@pytest.mark.parametrize(
    "before, after", [("", ""), ("b = 2.0 * b", ""), ("", "b = 2.0 * b")]
)
def test_block_new_scope(f2008_parser, before, after):
    """Test that a Block_Construct creates a new scoping region."""
    block = f2008_parser(
        get_reader(
            f"""\
            program foo
            integer :: b = 3
            {before}
            block
               integer :: b = 4
               a = 1 + b
            end block
            {after}
            end program foo
            """
        )
    )

    assert "BLOCK\nINTEGER :: b = 4\na = 1 + b\nEND BLOCK" in str(block).replace(
        "  ", ""
    )


def test_block_in_if(f2008_parser):
    """Test that a Block may appear inside an IF."""
    ptree = f2008_parser(
        get_reader(
            """\
            program foo
            integer :: b = 3
            if (b == 2) then
              block
                real :: tmp
                tmp = ATAN(0.5)
                b = NINT(tmp)
              end block
            end if
            end program foo
            """
        )
    )
    blocks = walk([ptree], Block_Construct)
    assert len(blocks) == 1


def test_named_block(f2008_create):
    """
    Test that a named block construct is correctly captured and also
    reproduced.
    """
    block = Block_Construct(
        get_reader(
            """\
            foo: block
               integer :: b = 4
               a = 1 + b
            end block foo
            """
        )
    )

    assert "foo:BLOCK\n  INTEGER :: b = 4\n  a = 1 + b\nEND BLOCK foo" in str(block)


def test_end_block_missing_start_name(f2008_create):  # C808
    with pytest.raises(FortranSyntaxError):
        Block_Construct(
            get_reader(
                """\
                block
                end block foo
                """
            )
        )


def test_end_block_missing_end_name(f2008_create):  # C808
    with pytest.raises(FortranSyntaxError):
        Block_Construct(
            get_reader(
                """\
                foo: block
                end block
                """
            )
        )


def test_end_block_wrong_name(f2008_create):  # C808
    with pytest.raises(FortranSyntaxError):
        Block_Construct(
            get_reader(
                """\
                foo: block
                end block bar
                """
            )
        )


def test_block_in_subroutine(f2008_parser):
    """Check that we get two, nested symbol tables when a subroutine contains
    a Block construct."""
    code = """\
            program my_prog
            real :: a
            a = -1.0
            if (a < 0.0) then
             rocking: block
               real :: b
               b = 42.0
               a = b
             end block rocking
            else
             a = 10.0
            end if
            end program my_prog
            """
    _ = f2008_parser(get_reader(code))
    tables = SYMBOL_TABLES
    assert list(tables._symbol_tables.keys()) == ["my_prog"]
    table = SYMBOL_TABLES.lookup("my_prog")
    assert len(table.children) == 1
    assert table.children[0].name == "rocking"
