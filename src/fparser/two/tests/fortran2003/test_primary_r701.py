# Copyright (c) 2019 Science and Technology Facilities Council

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

'''Test Fortran 2003 rule R701 : primary type.

'''
import pytest

import fparser.two.Fortran2003 as f2003
import fparser.two.utils


# A list of [Expression, Expected Type, ToString] for the test cases.
SUBCLASS_CASES = [
    ['1.2e-03', f2003.Real_Literal_Constant, '1.2E-03'],  # Constructor

    ['a%b', f2003.Data_Ref, 'a % b'],  # Designator
    ['[ 1.2, 2.3e+2, -5.1e-3 ]', f2003.Array_Constructor,  # Array constructor
     '[1.2, 2.3E+2, - 5.1E-3]'],
    ['PERSON  (12,  "Jones")', f2003.Part_Ref,
     'PERSON(12, "Jones")'],  # Structure constructor
    ['a', f2003.Name, 'a'],  # Type param name
    ['(a +  b)', f2003.Parenthesis, '(a + b)'], # Parenthesis (expr)
]


@pytest.mark.parametrize('source, expected_type, to_str', SUBCLASS_CASES)
def test_primary_subclasses(f2003_create, source, expected_type, to_str):
    '''
    '''
    obj = f2003.Primary(source)
    assert type(obj) == expected_type
    assert str(obj) == to_str


def test_no_match(f2003_create):
    '''
    '''
    with pytest.raises(fparser.two.utils.NoMatchError):
        obj = f2003.Primary('! A comment')
