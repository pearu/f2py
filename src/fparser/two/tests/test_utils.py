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

'''Test utils.py which contain base classes to support fparser,
exception handling and ast traversal.

'''

import pytest
from fparser.two.utils import FortranSyntaxError
from fparser.api import get_reader

# test BlockBase


def test_blockbase_match_names(f2003_create):
    '''Test the blockbase name matching option in its match method. We use
    the Derived_Type_Def class (which subclasses BlockBase) for this
    as it sets match_names to True.

    '''
    from fparser.two.Fortran2003 import Derived_Type_Def, Case_Construct

    # working named example
    reader = get_reader("type abc\nend type abc")
    ast = Derived_Type_Def(reader)
    assert "TYPE :: abc\nEND TYPE abc" in str(ast)

    # case insensitive
    reader = get_reader("type abc\nend type ABC")
    ast = Derived_Type_Def(reader)
    assert "TYPE :: abc\nEND TYPE ABC" in str(ast)

    # incorrect name exception
    reader = get_reader("type abc\nend type cde")
    with pytest.raises(FortranSyntaxError) as excinfo:
        ast = Derived_Type_Def(reader)
    assert "at line 2\n>>>end type cde\nExpecting name 'abc'" \
        in str(excinfo.value)

    # first name required if second name supplied
    # switch to using select case as it can trip the exception
    reader = get_reader("select case (i)\nend select label")
    with pytest.raises(FortranSyntaxError) as excinfo:
        ast = Case_Construct(reader)
    assert ("at line 2\n>>>end select label\nName 'label' has no "
            "corresponding starting name") in str(excinfo.value)


def test_blockbase_match_name_classes(f2003_create):
    '''Test the blockbase name matching option in its match method. We use
    the If_Construct class (which subclasses BlockBase) for this as it
    sets match_names to True and provides match_name_classes. This is
    used when names can appear in multiple places.

    '''
    from fparser.two.Fortran2003 import If_Construct

    # working named example
    reader = get_reader("label:if (.true.) then\nendif label")
    ast = If_Construct(reader)
    assert "label:IF (.TRUE.) THEN\nEND IF label" in str(ast)

    # case insensitive
    reader = get_reader("label:if (.true.) then\nendif LABEL")
    ast = If_Construct(reader)
    assert "label:IF (.TRUE.) THEN\nEND IF LABEL" in str(ast)

    # incorrect name exception
    reader = get_reader("label:if (.true.) then\nendif bella")
    with pytest.raises(FortranSyntaxError) as excinfo:
        ast = If_Construct(reader)
    assert "at line 2\n>>>endif bella\nExpecting name 'label'" \
        in str(excinfo.value)

    # first name required if subsequent name supplied
    reader = get_reader("if (.true.) then\nendif label")
    with pytest.raises(FortranSyntaxError) as excinfo:
        ast = If_Construct(reader)
    assert ("at line 2\n>>>endif label\nName 'label' has no corresponding "
            "starting name") in str(excinfo.value)


def test_get_child(f2003_create):
    ''' Test the get_child() utility. '''
    from fparser.two import Fortran2003
    from fparser.two.utils import get_child, walk_ast
    reader = get_reader("program hello\n"
                        "write(*,*) 'hello'\n"
                        "write(*,*) 'goodbye'\n"
                        "end program hello\n")
    main = Fortran2003.Program(reader)
    prog = get_child(main, Fortran2003.Main_Program)
    exe = get_child(prog, Fortran2003.Execution_Part)
    assert isinstance(exe, Fortran2003.Execution_Part)
    write_stmt = get_child(exe, Fortran2003.Write_Stmt)
    # Check that we got the first write and not the second
    assert "goodbye" not in str(write_stmt)
    # The top level has no Io_Control_Spec children
    assert not get_child(main, Fortran2003.Io_Control_Spec)
    # Check functionality when node has children in `items` and
    # not in `content`
    io_nodes = walk_ast(main.content, my_types=[Fortran2003.Io_Control_Spec])
    assert not hasattr(io_nodes[0], "content")
    io_unit = get_child(io_nodes[0], Fortran2003.Io_Unit)
    assert isinstance(io_unit, Fortran2003.Io_Unit)


@pytest.mark.usefixtures("f2003_create")
def test_parent_info():
    ''' Check that parent information is correctly set-up in the
    parse tree. '''
    import six
    from fparser.two import Fortran2003
    from fparser.two.utils import get_child, walk_ast, Base
    reader = get_reader("program hello\n"
                        "  implicit none\n"
                        "  integer :: var1, ji\n"
                        "  real(wp), dimension(10,10) :: var2\n"
                        "  write(*,*) 'hello'\n"
                        "  do ji = 1, var1\n"
                        "    var2(ji, 5) = -1.0\n"
                        "  end do\n"
                        "end program hello\n")
    main = Fortran2003.Program(reader)
    node_list = walk_ast([main], my_types=[Base])
    for node in node_list[1:]:
        if not isinstance(node, six.text_type):
            print(node)
            assert node.parent
    assert 0
