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
cases = [
    ['1.2e-03', f2003.Real_Literal_Constant, '1.2E-03'],
    ['(1, n)', f2003.Complex_Literal_Constant, '(1, n)'],
    ['.true.', f2003.Logical_Literal_Constant, '.TRUE.'],
    ['"hey a()c"', f2003.Char_Literal_Constant, '"hey a()c"'],
    ['b"0101"', f2003.Binary_Constant, 'B"0101"'],

    ['o"0107"', f2003.Octal_Constant, 'O"0107"'],
    ['z"a107"', f2003.Hex_Constant, 'Z"A107"'],
    ['a%b', f2003.Data_Ref, 'a % b'],
    ['[ 1.2, 2.3e+2, -5.1e-3 ]', f2003.Array_Constructor,
     '[1.2, 2.3E+2, - 5.1E-3]'],
    ['PERSON  (12,  "Jones")', f2003.Part_Ref,
     'PERSON(12, "Jones")'],
    ['a', f2003.Name, 'a'],
    ['(a)', f2003.Parenthesis, '(a)'],
    ['(a +  b)', f2003.Parenthesis, '(a + b)'],
]


@pytest.mark.parametrize('source, expected_type, to_str', cases)
def test_primary_subclasses(f2003_create, source, expected_type, to_str):
    obj = f2003.Primary(source)
    assert type(obj) == expected_type
    assert str(obj) == to_str


def test_no_match(f2003_create):
    with pytest.raises(fparser.two.utils.NoMatchError):
        obj = f2003.Primary('! A comment')
