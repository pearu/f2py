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

'''Test Fortran 2003 rule R427 : This file tests the support for a
character literal constant.

'''

import pytest
from fparser.two.Fortran2003 import Char_Literal_Constant
from fparser.two.utils import NoMatchError

def test_char_literal_constant():
    ''' Test that valid input is parsed correctly '''

    # simple, single quotes
    obj = Char_Literal_Constant("'DO'")
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == "'DO'"
    assert repr(obj) == 'Char_Literal_Constant("\'DO\'", None)'

    # simple, double quotes
    obj = Char_Literal_Constant('"DO"')
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == '"DO"'
    assert repr(obj) == 'Char_Literal_Constant(\'"DO"\', None)'

    # single quotes inside single quotes (two means one)
    obj = Char_Literal_Constant("'DON''T'")
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == "'DON''T'"

    # double quotes inside double quotes (two means one)
    obj = Char_Literal_Constant('"""busy"""')
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == '"""busy"""'

    # single quotes, spaces
    obj = Char_Literal_Constant("  '  D  O  '  ")
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == "'  D  O  '"
    assert repr(obj) == 'Char_Literal_Constant("\'  D  O  \'", None)'

    # Single quotes, empty string
    obj = Char_Literal_Constant("''")
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == "''"

    # Double quotes, empty string
    obj = Char_Literal_Constant('""')
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == '""'

    # include a kind parameter (which says what character set to
    # expect)
    obj = Char_Literal_Constant('NIH_"DO"')
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == 'NIH_"DO"'
    assert repr(obj) == 'Char_Literal_Constant(\'"DO"\', \'NIH\')'

    # additional characters
    obj = Char_Literal_Constant("'()!$%^&*_+=-01~@#;:/?.>,<|'")
    assert isinstance(obj, Char_Literal_Constant), repr(obj)
    assert str(obj) == "'()!$%^&*_+=-01~@#;:/?.>,<|'"

    # single quotes escaped inside single quotes is an error
    #obj = Char_Literal_Constant("'DON\'T'")
    #assert isinstance(obj, Char_Literal_Constant), repr(obj)
    #assert str(obj) == "'DON\'T'"

    # double quotes escaped inside double quotes
    #obj = Char_Literal_Constant('""important\""')
    #assert isinstance(obj, Char_Literal_Constant), repr(obj)
    #assert str(obj) == '"\"important\""'

def test_char_literal_constant_error():
    ''' Test that invalid input raises an exception '''

    # empty string
    for value in ["", None]:
        with pytest.raises(NoMatchError) as excinfo:
            obj = Char_Literal_Constant(value)
        assert "Char_Literal_Constant: '{0}'".format(value) in str(excinfo.value)

