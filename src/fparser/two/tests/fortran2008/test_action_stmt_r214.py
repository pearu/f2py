# Copyright (c) 2022 Science and Technology Facilities Council

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

'''Test Fortran 2008 rule R214

    action-stmt is allocate-stmt
                    or assignment-stmt
                    or backspace-stmt
                    or call-stmt
                    or close-stmt
                    or continue-stmt
                    or cycle-stmt
                    or deallocate-stmt
                    or end-function-stmt
                    or end-mp-subprogram-stmt
                    or end-program-stmt
                    or end-subroutine-stmt
                    or endfile-stmt
                    or error-stop-stmt
                    or exit-stmt
                    or flush-stmt
                    or forall-stmt
                    or goto-stmt
                    or if-stmt
                    or inquire-stmt
                    or lock-stmt
                    or nullify-stmt
                    or open-stmt
                    or pointer-assignment-stmt
                    or print-stmt
                    or read-stmt
                    or return-stmt
                    or rewind-stmt
                    or stop-stmt
                    or sync-all-stmt
                    or sync-images-stmt
                    or sync-memory-stmt
                    or unlock-stmt
                    or wait-stmt
                    or where-stmt
                    or write-stmt
                    or arithmetic-if-stmt
                    or computed-goto-stmt

    Associated constraints are:

    "C201 (R208) An execution-part shall not contain an end-function-stmt,
          end-mp-subprogram-stmt, end-program-stmt, or end-subroutine-stmt."
'''

import pytest
from fparser.api import get_reader, walk
from fparser.two.Fortran2003 import Stop_Stmt
from fparser.two.Fortran2008 import Action_Stmt, Error_Stop_Stmt
from fparser.two.utils import NoMatchError


@pytest.mark.usefixtures('f2008_create')
@pytest.mark.parametrize('string, cls', [
    ('STOP', Stop_Stmt)
])
def test_other(string, cls):
    '''Test that previous subclasses are still matched correctly.'''
    result = Action_Stmt(string)
    assert isinstance(result, cls)


def test_other_functional(f2008_parser):
    '''Test previous subclasses are still matched correctly in a subroutine.'''
    tree = f2008_parser(get_reader('''\
subroutine my_abort
stop
end subroutine my_abort
    '''))
    assert walk(tree, Stop_Stmt)
    assert 'STOP' in str(tree)


@pytest.mark.usefixtures('f2008_create')
@pytest.mark.parametrize('string, cls', [
    ('ERROR STOP', Error_Stop_Stmt)
])
def test_new_f2008_stmt(string, cls):
    '''Test that newly added F2008 subclasses are matched correctly.'''
    result = Action_Stmt(string)
    assert isinstance(result, cls)


@pytest.mark.usefixtures('f2008_create')
def test_error():
    '''Test that invalid syntax raises an error.'''
    with pytest.raises(NoMatchError) as excinfo:
        _ = Action_Stmt('ERR STOP')
    assert "Action_Stmt: 'ERR STOP'" in str(excinfo.value)
