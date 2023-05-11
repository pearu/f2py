# Copyright (c) 2018-2023 Science and Technology Facilities Council.

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

"""Test utils.py which contains base classes to support fparser,
exception handling and ast traversal.

"""

import os

import pytest
from fparser.api import get_reader
from fparser.common.readfortran import FortranFileReader
from fparser.two import Fortran2003, utils


# test BlockBase


@pytest.mark.usefixtures("f2003_create")
def test_blockbase_match_names():
    """Test the blockbase name matching option in its match method. We use
    the Derived_Type_Def class (which subclasses BlockBase) for this
    as it sets match_names to True.

    """
    # working named example
    reader = get_reader("type abc\nend type abc")
    ast = Fortran2003.Derived_Type_Def(reader)
    assert "TYPE :: abc\nEND TYPE abc" in str(ast)

    # case insensitive
    reader = get_reader("type abc\nend type ABC")
    ast = Fortran2003.Derived_Type_Def(reader)
    assert "TYPE :: abc\nEND TYPE ABC" in str(ast)

    # incorrect name exception
    reader = get_reader("type abc\nend type cde")
    with pytest.raises(utils.FortranSyntaxError) as excinfo:
        ast = Fortran2003.Derived_Type_Def(reader)
    assert "at line 2\n>>>end type cde\nExpecting name 'abc'" in str(excinfo.value)

    # first name required if second name supplied
    # switch to using select case as it can trip the exception
    reader = get_reader("select case (i)\nend select label")
    with pytest.raises(utils.FortranSyntaxError) as excinfo:
        ast = Fortran2003.Case_Construct(reader)
    assert (
        "at line 2\n>>>end select label\nName 'label' has no "
        "corresponding starting name"
    ) in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
def test_blockbase_match_name_classes():
    """Test the blockbase name matching option in its match method. We use
    the If_Construct class (which subclasses BlockBase) for this as it
    sets match_names to True and provides match_name_classes. This is
    used when names can appear in multiple places.

    """
    # working named example
    reader = get_reader("label:if (.true.) then\nendif label")
    ast = Fortran2003.If_Construct(reader)
    assert "label:IF (.TRUE.) THEN\nEND IF label" in str(ast)

    # case insensitive
    reader = get_reader("label:if (.true.) then\nendif LABEL")
    ast = Fortran2003.If_Construct(reader)
    assert "label:IF (.TRUE.) THEN\nEND IF LABEL" in str(ast)

    # incorrect name exception
    reader = get_reader("label:if (.true.) then\nendif bella")
    with pytest.raises(utils.FortranSyntaxError) as excinfo:
        ast = Fortran2003.If_Construct(reader)
    assert "at line 2\n>>>endif bella\nExpecting name 'label'" in str(excinfo.value)

    # first name required if subsequent name supplied
    reader = get_reader("if (.true.) then\nendif label")
    with pytest.raises(utils.FortranSyntaxError) as excinfo:
        ast = Fortran2003.If_Construct(reader)
    assert (
        "at line 2\n>>>endif label\nName 'label' has no corresponding " "starting name"
    ) in str(excinfo.value)


@pytest.mark.usefixtures("f2003_create")
def test_endstmtbase_match():
    """Tests for the EndStmtBase.match() method."""
    result = utils.EndStmtBase.match("critical", None, "hello")
    assert result is None
    # No statement type is required by default
    result = utils.EndStmtBase.match("CRITICAL", None, "end")
    assert result == (None, None)
    # Missing statement type.
    result = utils.EndStmtBase.match("CRITICAL", None, "end", require_stmt_type=True)
    assert result is None
    # Matching statement type.
    result = utils.EndStmtBase.match(
        "CRITICAL", None, "end  critical", require_stmt_type=True
    )
    assert result == ("CRITICAL", None)
    # End construct with name but no class to match it with.
    result = utils.EndStmtBase.match(
        "SUBROUTINE", None, "end  subroutine sub", require_stmt_type=True
    )
    assert result is None
    # End construct with name that matches with supplied class.
    result = utils.EndStmtBase.match(
        "SUBROUTINE",
        Fortran2003.Subroutine_Name,
        "end  subroutine sub",
        require_stmt_type=True,
    )
    assert result == ("SUBROUTINE", Fortran2003.Name("sub"))


@pytest.mark.usefixtures("f2003_create")
def test_base_include_handling(tmpdir):
    """Test parsing of code that involves an INCLUDE. This is beyond what
    is tested in test_readfortran as it exercises the mechanisms used when
    a match fails and content is given back to the reader."""
    fortran_code = (
        "module include_test\n  include 'test_include.h'\nend module include_test"
    )
    include_code = (
        "interface mpi_sizeof\n"
        "subroutine simple()\n"
        "end subroutine simple\n"
        "end interface mpi_sizeof"
    )
    with open(os.path.join(tmpdir, "test_prog.f90"), "w", encoding="utf-8") as cfile:
        cfile.write(fortran_code)
    with open(os.path.join(tmpdir, "test_include.h"), "w", encoding="utf-8") as cfile:
        cfile.write(include_code)
    reader = FortranFileReader(
        os.path.join(tmpdir, "test_prog.f90"), include_dirs=[tmpdir]
    )
    prog = Fortran2003.Program(reader)
    assert isinstance(prog, Fortran2003.Program)
    nodes = utils.walk(prog, Fortran2003.Interface_Stmt)
    assert len(nodes) == 1
