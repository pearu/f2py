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

'''Test Fortran 2003 rule R1101 : This file tests the support for a
main program with a program statement. It does not test the case where
there is no program statement. That situation is covered by the
Main_Program0 class. Tests for Main_Program0 are currently in
test_fortran2003.py.

'''

import pytest
from fparser.two.utils import FortranSyntaxError
from fparser.api import get_reader
from fparser.two.Fortran2003 import Main_Program

# Test single program units

def test_main_program(f2003_create):

    # basic
    obj = Main_Program(get_reader("program a\nend"))
    assert isinstance(obj, Main_Program)
    assert str(obj) == 'PROGRAM a\nEND PROGRAM a'
    assert repr(obj) == ("Main_Program(Program_Stmt('PROGRAM', Name('a')), "
                         "End_Program_Stmt('PROGRAM', None))")

    # basic plus end program
    obj = Main_Program(get_reader("program a\nend program"))
    assert str(obj) == 'PROGRAM a\nEND PROGRAM a'

    # basic plus end program name
    obj = Main_Program(get_reader("program a\nend program a"))
    assert str(obj) == 'PROGRAM a\nEND PROGRAM a'

    # spaces
    obj = Main_Program(get_reader("  program  a  \n  end  program  a  "))
    assert str(obj) == 'PROGRAM a\nEND PROGRAM a'

    # mixed case
    obj = Main_Program(get_reader("pRoGrAm A\nEnD PrOgRaM a"))
    assert str(obj) == 'PROGRAM A\nEND PROGRAM a'

    # specification-part
    obj = Main_Program(get_reader("program a\ninteger i\nend program a"))
    assert str(obj) == 'PROGRAM a\n  INTEGER :: i\nEND PROGRAM a'
    
    # execution-part
    obj = Main_Program(get_reader("program a\ni=10\nend program a"))
    assert str(obj) == 'PROGRAM a\n  i = 10\nEND PROGRAM a'

    # internal-subprogram-part
    obj = Main_Program(get_reader("program a\ncontains\nsubroutine foo\n"
                                  "end\nend program a"))
    assert str(obj) == ("PROGRAM a\n  CONTAINS\n  SUBROUTINE foo\n"
                        "  END SUBROUTINE foo\nEND PROGRAM a")

    # specification-part + execution-part
    obj = Main_Program(get_reader("program a\ninteger i\ni=10\nend program a"))
    assert str(obj) == 'PROGRAM a\n  INTEGER :: i\n  i = 10\nEND PROGRAM a'
    
    # execution-part + internal-subprogram-part
    obj = Main_Program(get_reader("program a\ni=10\ncontains\nsubroutine foo\n"
                                  "end\nend program a"))
    assert str(obj) == ("PROGRAM a\n  i = 10\n  CONTAINS\n  SUBROUTINE foo\n"
                        "  END SUBROUTINE foo\nEND PROGRAM a")

    # specification-part + execution-part + internal-subprogram-part
    obj = Main_Program(get_reader("program a\ninteger i\ni=10\ncontains\n"
                                  "subroutine foo\nend\nend program a"))
    assert str(obj) == ("PROGRAM a\n  INTEGER :: i\n  i = 10\n  CONTAINS\n  "
                        "SUBROUTINE foo\n  END SUBROUTINE foo\nEND PROGRAM a")
