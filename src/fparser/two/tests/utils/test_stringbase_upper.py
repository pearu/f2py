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

'''File containing unit tests for the STRINGBase baseclass in
utils.py'''

from fparser.two.utils import STRINGBase


def test_stringbase_string():
    '''Test the STRINGbase match method with a string pattern.'''

    pattern = "HELLO"
    for my_input in ["hello", "HeLlO", "HELLO"]:
        result = STRINGBase.match(pattern, my_input)
        assert repr(result) == "('{0}',)".format(pattern)
        assert str(result[0]) == pattern


def test_stringbase_re():
    '''Test the STRINGbase match method with a regular expression.'''

    import re
    pattern = re.compile('[A-Z][0-9]+')
    for my_input in ["a123", "A123"]:
        result = STRINGBase.match(pattern, my_input)
        assert repr(result) == "('{0}',)".format(my_input.upper())
        assert str(result[0]) == my_input.upper()


def test_stringbase_list():
    '''Test the STRINGbase match method with a list.'''

    import re
    pattern1 = re.compile('[A-Z][0-9]+')
    pattern2 = "HELLO"
    pattern_list = [pattern1, pattern2]
    for my_input in ["a123", "A123", "hello", "HeLlO", "HELLO"]:
        result = STRINGBase.match(pattern_list, my_input)
        assert repr(result) == "('{0}',)".format(my_input.upper())
        assert str(result[0]) == my_input.upper()


def test_stringbase_tuple():
    '''Test the STRINGbase match method with a tuple.'''

    import re
    pattern1 = re.compile('[A-Z][0-9]+')
    pattern2 = "HELLO"
    pattern_tuple = (pattern1, pattern2)
    for my_input in ["a123", "A123", "hello", "HeLlO", "HELLO"]:
        result = STRINGBase.match(pattern_tuple, my_input)
        assert repr(result) == "('{0}',)".format(my_input.upper())
        assert str(result[0]) == my_input.upper()


def test_stringbase_pattern_class():
    '''Test the STRINGbase match method with a pattern instance as
    specified in pattern_tools.py.

    '''

    from fparser.two import pattern_tools
    pattern = pattern_tools.intrinsic_type_name
    for my_input in ["logical", "LoGiCaL", "LOGICAL"]:
        result = STRINGBase.match(pattern, my_input)
        assert repr(result) == "('{0}',)".format(my_input.upper())
        assert str(result[0]) == my_input.upper()
