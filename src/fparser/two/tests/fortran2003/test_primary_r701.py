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

Sub-rules:
    C701 (R701) The type-param-name shall be the name of a type parameter.
    C702 (R701) The designator shall not be a whole assumed-size array

Neither C701 nor C702 can be tested here, as they require context of the
type defined outside of Primary.

'''
import sys

import pytest

import fparser.two.Fortran2003 as f2003
import fparser.two.utils


def assert_subclass_parse(source, base_type, actual_type=None,
                          expected_str=None):

    obj = f2003.Primary(source)
    assert type(obj) in possible_subclasses(base_type)

    if actual_type:
        assert isinstance(obj, actual_type)
    else:
        assert isinstance(obj, base_type)

    if expected_str:
        assert expected_str == str(obj)


def test_Constant(f2003_create):
    assert_subclass_parse(
        '1.2e-03', f2003.Constant,
        actual_type=f2003.Real_Literal_Constant,
        expected_str='1.2E-03')


def test_Designator(f2003_create):
    assert_subclass_parse(
        'array(1:5)', f2003.Designator,
        actual_type=f2003.Array_Section,
        expected_str='array(1 : 5)')


def test_Array_Constructor(f2003_create):
    assert_subclass_parse(
        '[ 1.2, 2.3e + 2,    -5.1 e-3 ]', f2003.Array_Constructor,
        actual_type=f2003.Array_Constructor,
        expected_str='[1.2, 2.3E+2, - 5.1E-3]')


def test_Structure_Constructor(f2003_create):
    # The actual type is Part_Ref - it is possible that this could
    # change in the future.
    assert_subclass_parse(
        'PERSON ( 12,   "Jones" )', f2003.Structure_Constructor,
        actual_type=f2003.Part_Ref,
        expected_str='PERSON(12, "Jones")')


@pytest.mark.xfail(reason="Unable to reach Function_Reference when parsing")
def test_Function_Reference(f2003_create):
    '''This test demonstrates the inability to distinguish
    Structure_Constructor from Function_Reference without more parse context
    than is currently being provided.
    '''
    assert_subclass_parse(
        'a_function(1.2, some_kwarg="hello")', f2003.Function_Reference)


@pytest.mark.xfail
def test_Type_Param_Inquiry():
    '''This test demonstrates the inability to distinguish Designator from
    Type_Param_Inquiry without more parse context than is currently being
    provided.
    '''
    assert_subclass_parse(
        'an_object % a_type_bound_function', f2003.Type_Param_Inquiry)


@pytest.mark.xfail
def test_Type_Param_Name():
    '''This test demonstrates the inability to distinguish Constant from
    Type_Param_Name without more parse context than is currently being
    provided.
    '''
    assert_subclass_parse(
        'A_TYPE', f2003.Type_Param_Name)


def test_Parenthesis(f2003_create):
    assert_subclass_parse(
        '(a +  b)', f2003.Parenthesis,
        expected_str='(a + b)')


def possible_subclasses(node_type, _seen=None):
    '''Given a type, return all of the subtypes that could
    have been matched.
    '''
    seen = _seen or []
    subclasses = getattr(node_type, 'subclass_names', [])
    if node_type not in seen:
        seen.append(node_type)

    for subclass_name in subclasses:
        module = sys.modules[node_type.__module__]
        subclass = getattr(module, subclass_name, None)
        if subclass is not None and subclass not in seen:
            seen.append(subclass)
            possible_subclasses(subclass, seen)
    return seen


def test_no_match(f2003_create):
    '''Test that a NoMatchError is raised if we provide code
    that isn't allowed as a Primary type (e.g. a comment).
    '''
    with pytest.raises(fparser.two.utils.NoMatchError):
        obj = f2003.Primary('! A comment')


@pytest.mark.xfail
def test_C702_no_assumed_size_array(f2003_create):
    '''Test C702 (R701) The designator shall not be a whole assumed-size array.
    This test cannot be passed without more parse context of things like
    defined types.
    '''
    source = """
        integer(*)  :: assumed_size_array, result
        result = assumed_size_array
    """
    # TODO: Implement the actual parsing of this source.
    assert source is None
