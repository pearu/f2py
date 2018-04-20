# Copyright (c) 2017-2018 Science and Technology Facilities Council
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

''' Module containing tests for aspects of fparser2 related to comments '''

import pytest
from fparser.two.Fortran2003 import Program, Comment, Subroutine_Subprogram, \
    walk_ast
from fparser.api import get_reader


def test_ignore_comments():
    ''' Check that the parser does throw away comments when requested '''
    tree = Program(get_reader('''\
PROGRAM a_prog
! A full line comment
PRINT *, "Hello" ! This block gets executed
END PROGRAM a_prog
    ''', ignore_comments=True))
    gen = str(tree)
    assert ("PROGRAM a_prog\n"
            "  PRINT *, \"Hello\"\n"
            "END PROGRAM a_prog" in gen)
    assert "full line comment" not in gen
    assert "block gets executed" not in gen

    # Check that the default behaviour is to ignore comments
    tree = Program(get_reader('''\
PROGRAM a_prog
! A full line comment
PRINT *, "Hello" ! This block gets executed
END PROGRAM a_prog
    '''))
    gen = str(tree)
    assert "full line comment" not in gen
    assert "block gets executed" not in gen


def test_simple_prog():
    ''' Tests simplest case of comments in a program unit '''
    tree = Program(get_reader('''\
PROGRAM a_prog
! A full line comment
PRINT *, "Hello" ! This block gets executed
END PROGRAM a_prog
    ''', ignore_comments=False))
    assert (str(tree) == "PROGRAM a_prog\n"
            "  ! A full line comment\n"
            "  PRINT *, \"Hello\"\n"
            "  ! This block gets executed\n"
            "END PROGRAM a_prog\n")


def test_ifthen():
    ''' Tests for comments within an if-then block '''
    cls = Program
    tree = cls(get_reader('''\
PROGRAM a_prog
IF(.TRUE.)THEN
! A full line comment
PRINT *, "Hello"
! Another full line comment
END IF
END PROGRAM a_prog
    ''', ignore_comments=False))
    assert (str(tree) == "PROGRAM a_prog\n"
            "  IF (.TRUE.) THEN\n"
            "    ! A full line comment\n"
            "    PRINT *, \"Hello\"\n"
            "    ! Another full line comment\n"
            "  END IF\n"
            "END PROGRAM a_prog\n")


def test_inline_ifthen():
    ''' Tests for in-line comments within an if-then block '''
    tree = Program(get_reader('''\
PROGRAM a_prog
IF(.TRUE.)THEN
PRINT *, "Hello" ! An in-line comment here
END IF
! A comment after a block
END PROGRAM a_prog
    ''', ignore_comments=False))
    assert isinstance(tree, Program)
    assert (str(tree) == "PROGRAM a_prog\n"
            "  IF (.TRUE.) THEN\n"
            "    PRINT *, \"Hello\"\n"
            "    ! An in-line comment here\n"
            "  END IF\n"
            "  ! A comment after a block\n"
            "END PROGRAM a_prog\n")

@pytest.mark.xfail(reason="fails to preserve formatting in fixed format")
def test_fixed_fmt():
    ''' Test that we handle comments in fixed-format mode '''
    reader = get_reader('''\
      subroutine foo
c this is a subroutine
        end subroutine foo''', isfree=False, ignore_comments=True)
    cls = Subroutine_Subprogram
    obj = cls(reader)
    assert isinstance(obj, cls), repr(obj)
    assert str(obj) == 'SUBROUTINE foo\nEND SUBROUTINE foo'
    assert (repr(obj) == "Subroutine_Subprogram(Subroutine_Stmt(None, "
            "Name('foo'), None, None), End_Subroutine_Stmt('SUBROUTINE', "
            "Name('foo')))")
    reader = get_reader('''\
      subroutine foo
c this is a subroutine
      end subroutine foo''', isfree=False, ignore_comments=False)
    cls = Subroutine_Subprogram
    obj = cls(reader)
    fort = obj.tofortran(isfix=True)
    assert isinstance(obj, cls), repr(obj)
    assert (fort == '      SUBROUTINE foo\nc this is a subroutine\n'
            '      END SUBROUTINE foo')
    assert (repr(obj) == "Subroutine_Subprogram(Subroutine_Stmt(None, "
            "Name('foo'), None, None), End_Subroutine_Stmt('SUBROUTINE', "
            "Name('foo')))")


@pytest.mark.xfail(reason="fails to preserve formatting in fixed format")
def test_fixed_continuation():
    ''' Check that we handle comments that occur within fixed-format
    continued lines '''
    reader = get_reader('''\
      subroutine foo
c this is a subroutine
      i = 1 +
c this comment is a problem
     &2 + 3
      end subroutine foo''', isfree=False, ignore_comments=False)
    cls = Subroutine_Subprogram
    obj = cls(reader)
    assert isinstance(obj, cls)
    print str(obj)
    assert "i = 1 + 2 + 3\n     c this comment is a problem" in str(obj)


def test_prog_comments():
    ''' Unit tests for lines in programs containing comments '''
    cls = Program
    reader = get_reader('''\
   ! A troublesome comment
   program foo
     ! A full comment line
     write(*,*) my_int ! An in-line comment
    end program foo
! A really problematic comment
''', isfree=True, ignore_comments=False)

    obj = cls(reader)
    assert type(obj) == Program
    # Check that the AST has the expected structure:
    # Program
    #   |--> Comment
    #   |--> Main_Program
    #   .    |--> Program_Stmt
    #   .    |--> Specification_Part
    #   .    .    \--> Implicit_Part
    #   .    .         \--> Comment
    #        |--> Execution_Part
    #        |    |--> Write_Stmt
    #        |    \--> Comment
    #   .    .
    #   .
    #   |--> Comment
    from fparser.two.Fortran2003 import Main_Program, Write_Stmt, \
        End_Program_Stmt
    walk_ast(obj.content, [Comment], debug=True)
    assert type(obj.content[0]) == Comment
    assert str(obj.content[0]) == "! A troublesome comment"
    assert type(obj.content[1]) == Main_Program
    main_prog = obj.content[1]
    assert type(main_prog.content[1].content[0].content[0]) == Comment
    assert (str(main_prog.content[1].content[0].content[0]) ==
            "! A full comment line")
    exec_part = main_prog.content[2]
    assert type(exec_part.content[0]) == Write_Stmt
    # Check that we have the in-line comment as a second statement
    assert len(exec_part.content) == 2
    assert type(exec_part.content[1]) == Comment
    assert type(main_prog.content[3]) == End_Program_Stmt
    assert "! An in-line comment" in str(obj)
    # Check that we still have the ending comment
    assert type(obj.content[-1]) == Comment
    print str(obj)
    assert str(obj).endswith("! A really problematic comment")


def test_module_comments():
    ''' Tests for comments in modules '''
    source = '''! This is a module
      module my_mod
        implicit none
        private
      end module my_mod
'''
    # Test when the reader is explicitly set to free-form mode
    reader = get_reader(source, isfree=True, ignore_comments=False)
    prog_unit = Program(reader)
    walk_ast(prog_unit.content, debug=True)
    assert type(prog_unit.content[0]) == Comment
    assert str(prog_unit.content[0]) == "! This is a module"
