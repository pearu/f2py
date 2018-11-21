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


def test_char_literal_constant():  # R427

    tcls = Char_Literal_Constant
    obj = tcls('NIH_"DO"')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'NIH_"DO"'
    assert repr(obj) == 'Char_Literal_Constant(\'"DO"\', \'NIH\')'

    obj = tcls("'DO'")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "'DO'"
    assert repr(obj) == 'Char_Literal_Constant("\'DO\'", None)'

    obj = tcls("'DON''T'")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "'DON''T'"

    obj = tcls('"DON\'T"')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == '"DON\'T"'

    obj = tcls('""')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == '""'

    obj = tcls("''")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "''"

    obj = tcls('"hey ha(ada)\t"')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == '"hey ha(ada)\t"'
