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

'''File containing unit tests for the SequenceBase baseclass in
utils.py'''

import pytest
from fparser.two.utils import SequenceBase, InternalError
from fparser.two.Fortran2003 import Name, Entity_Decl


def test_match_invalid_separator(f2003_create):
    '''Test the sequencebase match method raises the expected exception if
    arg separator is not a string.

    '''
    for separator in [None, 12, Name]:
        with pytest.raises(InternalError) as excinfo:
            _ = SequenceBase.match(separator, Name, "hello")
        assert ("SequenceBase class match method argument separator expected "
                "to be a string but found " in str(excinfo.value))


def test_match_invalid_string(f2003_create):
    '''Test the sequencebase match method raises the expected exception if
    arg string is not a string.

    '''
    for string in [None, 12, Name]:
        with pytest.raises(InternalError) as excinfo:
            _ = SequenceBase.match(",", Name, string)
        assert ("SequenceBase class match method argument string expected to "
                "be a string but found " in str(excinfo.value))


def test_match(f2003_create):
    '''Test the sequencebase match method with 1 or more entries.'''
    separator = ","
    subcls = Name
    string = "a"
    result = SequenceBase.match(separator, subcls, string)
    assert str(result) == "(',', (Name('a'),))"
    string = "a,b"
    result = SequenceBase.match(separator, subcls, string)
    assert str(result) == "(',', (Name('a'), Name('b')))"
    string = "a,b,c"
    result = SequenceBase.match(separator, subcls, string)
    assert str(result) == "(',', (Name('a'), Name('b'), Name('c')))"


def test_match_repmap(f2003_create):
    '''Test the sequencebase match method works if the supplied separator
    is also used within the matching class.

    '''
    from fparser.two.Fortran2003 import Entity_Decl_List
    separator = ","
    subcls = Entity_Decl_List
    string = "a(n, n), b(n, n)"
    result = SequenceBase.match(separator, subcls, string)
    assert (str(result).replace('u', '') ==
            "(',', (Entity_Decl_List(',', (Entity_Decl(Name('a'), "
            "Explicit_Shape_Spec_List(',', (Explicit_Shape_Spec(None, "
            "Name('n')), Explicit_Shape_Spec(None, Name('n')))), None, "
            "None),)), Entity_Decl_List(',', (Entity_Decl(Name('b'), "
            "Explicit_Shape_Spec_List(',', (Explicit_Shape_Spec(None, "
            "Name('n')), Explicit_Shape_Spec(None, Name('n')))), None, "
            "None),))))")


def test_match_matchempty(f2003_create):
    '''Test the sequencebase match method matches when the separator is a
    space and there are multiple spaces between the items.

    '''
    separator = " "
    subcls = Name
    # Multiple spaces should match
    options = ["a b", "a  b", " a    b "]
    for string in options:
        result = SequenceBase.match(separator, subcls, string)
        assert str(result) == "(' ', (Name('a'), Name('b')))"


def test_match_instance(f2003_create):
    '''Test the sequencebase init, tostr and torepr methods.'''
    from fparser.two.Fortran2003 import Data_Ref, Prefix, Type_Param_Name_List

    # ' ' separator.
    obj = Prefix("ELEMENTAL  IMPURE RECURSIVE")
    assert obj.tostr() == "ELEMENTAL IMPURE RECURSIVE"
    assert (obj.torepr() ==
            "Prefix(' ', (Prefix_Spec('ELEMENTAL'), Prefix_Spec('IMPURE'), "
            "Prefix_Spec('RECURSIVE')))")

    # ',' separator.
    obj = Type_Param_Name_List("a,b,c")
    assert obj.tostr() == "a, b, c"
    assert (obj.torepr() ==
            "Type_Param_Name_List(',', (Name('a'), Name('b'), Name('c')))")

    # Separator that is not ',' or ' '
    obj = Data_Ref("a%b%c")
    assert obj.tostr() == "a % b % c"
    assert obj.torepr() == "Data_Ref('%', (Name('a'), Name('b'), Name('c')))"


def test_match_repmap_spaces(f2003_create):
    '''Test the sequencebase match method matches when the separator is a
    space, repmap is required and there are multiple spaces between
    the items. This situation does not currently occur in the existing
    Fortran classes but should be checked in any case.

    '''
    separator = " "
    subcls = Entity_Decl
    # Multiple spaces with repmap tuples should match
    options = ["a(n(1)) b(n(2))", "a(n(1))  b(n(2))", " a(n(1))    b(n(2)) "]
    for string in options:
        result = SequenceBase.match(separator, subcls, string)
        assert str(result).replace('u', '') == (
            "(' ', (Entity_Decl(Name('a'), Explicit_Shape_Spec_List(',', "
            "(Explicit_Shape_Spec(None, Part_Ref(Name('n'), "
            "Section_Sbscript_List(',', (Int_Literal_Constant('1', "
            "None),)))),)), None, None), Entity_Decl(Name('b'), "
            "Explicit_Shape_Spec_List(',', (Explicit_Shape_Spec(None, "
            "Part_Ref(Name('n'), Section_Sbscript_List(',', "
            "(Int_Literal_Constant('2', None),)))),)), None, None)))")
