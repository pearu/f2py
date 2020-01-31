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

'''Test Fortran 2003 rule R603 : This file tests support for the
Designator class.

Three of the rules in Designator class are currently not called as
they are matched earlier. Some rule constraints (if implemented, which
they are not, as #201 must be done first) would improve things but
potentially not fully. Here is note 6.6 from the F2003 specification
which helps clarify for derived types:

The syntax rules are structured such that a data-ref that ends in a
component name without a following subscript list is a structure
component, even when other component names in the dataref are followed
by a subscript list. A data-ref that ends in a component name with a
following subscript list is either an array element or an array
section. A data-ref of nonzero rank that ends with a substring-range
is an array section. A data-ref of zero rank that ends with a
substring-range is a substring.

'''
import pytest
from fparser.two.Fortran2003 import Designator, Name, Part_Ref, \
    Array_Section, Program, Substring, Data_Ref
from fparser.api import get_reader


@pytest.mark.usefixtures("f2003_create")
def test_object_name():
    '''Test that an Name object is returned when a valid object-name name
    is supplied

    '''
    result = Designator("fred")
    assert str(result) == "fred"
    assert isinstance(result, Name)


@pytest.mark.usefixtures("f2003_create")
def test_array_element():
    '''Test that a Part_Ref object is returned when a valid array-element
    is supplied

    '''
    result = Designator("a(:)")
    assert str(result) == "a(:)"
    assert isinstance(result, Part_Ref)


@pytest.mark.usefixtures("f2003_create")
def test_array_section():
    '''Test that an Array_Section object is returned when a valid
    array-section is supplied.

    '''
    result = Designator("a(:)(:)")
    assert str(result) == "a(:)(:)"
    assert isinstance(result, Array_Section)


@pytest.mark.xfail(reason="issue #201 Array section is parsed as an array "
                   "element.")
@pytest.mark.usefixtures("f2003_create")
def test_array_section2():
    '''Test that an Array_Section object is returned when a valid
    array-element is supplied. This fails as the example matches with
    array-element and the constraints are not checked.

    There are various constraints in Array_Section which might
    distinguish between the rules but I wonder whether a different
    ordering would be required as I am not sure that the constraints
    on earlier rules avoid overlaps with later rules.

    '''
    reader = get_reader(
        "program test\n"
        "character(len=10) :: a\n"
        "a(1:3)='hey'\n"
        "end program test\n")
    result = Program(reader)
    assert "a(1 : 3) = 'hey'" in str(result)
    assert isinstance(result, Program)
    assert "Array_Section" in repr(result)


@pytest.mark.usefixtures("f2003_create")
def test_structure_component():
    '''Test that a Structure_Component object is returned when a valid
    structure-component is supplied.

    *** This rule is never called as it has the same structure as
    Array_Element which precedes it in the list and is therefore
    matched first ***

    It is a structure component if within a structure and there is
    more than one part_ref. However, it does not seem to matter as we
    get the expected result in any case.

    '''
    result = Designator("parent%scalar_field")
    assert str(result) == "parent % scalar_field"
    assert isinstance(result, Data_Ref)


@pytest.mark.xfail(reason="issue #201 substring is parsed as an array "
                   "element.")
@pytest.mark.usefixtures("f2003_create")
def test_substring():
    '''Test that a Substring object is returned when a valid
    substring is supplied.

    *** This rule is never called as it has the same structure as
    Array_Section which precedes it in the list and is therefore
    matched first. However, this in turn is not called as it is
    preceded by Array_Element which is matched first. ***

    There are various constraints in Substring which might distinguish
    between the rules but I wonder whether a different ordering would
    be required as I am not sure that the constraints on earlier rules
    avoid overlap with later rules.

    '''
    result = Designator("parent_string(1:2)")
    assert str(result) == "parent_string(1 : 2)"
    assert isinstance(result, Substring)
