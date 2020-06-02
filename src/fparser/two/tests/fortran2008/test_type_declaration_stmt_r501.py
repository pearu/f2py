# Copyright (c) 2020 Science and Technology Facilities Council

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

'''Test Fortran 2008 rule R501.

    type-declaration-stmt is declaration-type-spec [ [ , attr-spec ] ... :: ]
                             entity-decl-list

'''

from fparser.two.Fortran2008 import Type_Declaration_Stmt


def test_type_declaration_stmt():  # R501
    '''
    Tests copied from Fortran 2003.

    '''
    tcls = Type_Declaration_Stmt
    obj = tcls('integer a')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'INTEGER :: a'
    assert (repr(obj) ==
            "Type_Declaration_Stmt(Intrinsic_Type_Spec('INTEGER', "
            "None), None, Entity_Decl_List(',', (Entity_Decl(Name('a'), None, "
            "None, None),)))")

    obj = tcls('integer ,dimension(2):: a*3')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'INTEGER, DIMENSION(2) :: a*3'

    obj = tcls('real a')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'REAL :: a'
    assert (repr(obj) ==
            "Type_Declaration_Stmt(Intrinsic_Type_Spec('REAL', None), None, "
            "Entity_Decl_List(',', (Entity_Decl(Name('a'), None, None, "
            "None),)))")

    obj = tcls('REAL A( LDA, * ), B( LDB, * )')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'REAL :: A(LDA, *), B(LDB, *)'

    obj = tcls('DOUBLE PRECISION   ALPHA, BETA')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'DOUBLE PRECISION :: ALPHA, BETA'

    obj = tcls('logical,parameter:: T=.true.')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'LOGICAL, PARAMETER :: T = .TRUE.'

    obj = tcls('character(n),private:: x(n)')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'CHARACTER(LEN = n), PRIVATE :: x(n)'

    obj = tcls('character(lenmax),private:: x(n)')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'CHARACTER(LEN = lenmax), PRIVATE :: x(n)'
