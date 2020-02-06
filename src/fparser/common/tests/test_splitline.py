# Modified work Copyright (c) 2017 Science and Technology Facilities Council
# Original work Copyright (c) 1999-2008 Pearu Peterson

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

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

# Original author: Pearu Peterson <pearu@cens.ioc.ee>
# First version created: May 2006
# Modified by J. Henrichs, Bureau of Meteorology <joerg.henrichs@bom.gov.au>

"""
Test parsing single Fortran lines.

"""

from fparser.common.splitline import splitparen, splitquote, string_replace_map


def test_splitparen():
    ''' Unit tests for splitparen function.'''
    assert splitparen('abc') == ['abc']
    assert splitparen('abc(1)') == ['abc', '(1)']
    assert splitparen('abc(1) xyz') == ['abc', '(1)', ' xyz']
    assert splitparen('a(b) = b(x,y(1)) b((a))') == \
        ['a', '(b)', ' = b', '(x,y(1))', ' b', '((a))']
    # pylint: disable=anomalous-backslash-in-string
    assert splitparen('a(b) = b(x,y(1)) b\((a)\)') == \
        ['a', '(b)', ' = b', '(x,y(1))', ' b\\(', '(a)', '\\)']
    # pylint: enable=anomalous-backslash-in-string
    assert splitparen('abc[1]') == ['abc', '[1]']
    assert splitparen('abc[1,2,3]') == ['abc', '[1,2,3]']
    assert splitparen('a[b] = b[x,y(1)] b((a))') == \
        ['a', '[b]', ' = b', '[x,y(1)]', ' b', '((a))']
    # pylint: disable=anomalous-backslash-in-string
    assert splitparen('a[b] = b[x,y(1)] b\((a)\)') == \
        ['a', '[b]', ' = b', '[x,y(1)]', ' b\\(', '(a)', '\\)']
    # pylint: enable=anomalous-backslash-in-string
    assert splitparen('integer a(3) = (/"a", "b", "c"/)') == \
        ['integer a', '(3)', ' = ', '(/"a", "b", "c"/)']
    assert splitparen(
        'character(len=40) :: a(3) = (/"a[),", ",b,[(", "c,][)("/)') == \
        ['character', '(len=40)', ' :: a', '(3)', ' = ',
         '(/"a[),", ",b,[(", "c,][)("/)']
    assert splitparen('integer a(3) = ["a", "b", "c"]') == \
        ['integer a', '(3)', ' = ', '["a", "b", "c"]']
    assert splitparen(
        'character(len=40) :: a(3) = ["a[),", ",b,[(", "c,][)("]') == \
        ['character', '(len=40)', ' :: a', '(3)', ' = ',
         '["a[),", ",b,[(", "c,][)("]']
    # pylint: disable=anomalous-backslash-in-string
    result = splitparen('a(1),b\\((2,3),c\\\\((1)),c"("')
    expected = ['a', '(1)', ',b\\(', '(2,3)', ',c\\\\', '((1))', ',c"("']
    # pylint: enable=anomalous-backslash-in-string
    assert result == expected
    # Useful for debugging:
    # for i in range(len(EXPECTED)):
    #     print i,l[i],EXPECTED[i],l[i]==EXPECTED[i]


def test_splitquote():
    '''Tests splitquote function.'''
    split_list, stopchar = splitquote('abc\\\' def"12\\"3""56"dfad\'a d\'')
    assert split_list == ['abc\\\' def', '"12\\"3"', '"56"', 'dfad', '\'a d\'']
    assert stopchar is None
    result, stopchar = splitquote('abc\\\' def"12\\"3""56"dfad\'a d\'')
    assert result == ['abc\\\' def', '"12\\"3"', '"56"', 'dfad', '\'a d\'']
    assert stopchar is None

    split_list, stopchar = splitquote('a\'')
    assert split_list == ['a', '\'']
    assert stopchar == '\''

    split_list, stopchar = splitquote('a\'b')
    assert split_list == ['a', '\'b']
    assert stopchar == '\''


def test_string_replace_map():
    '''Tests string_replace_map function.'''
    result, string_map = string_replace_map('a()')
    assert result == 'a()'
    assert string_map == {}
