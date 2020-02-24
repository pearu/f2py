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

'''Test Fortran 2008 rule R437.

    <component-attr-spec> = <access-spec>
                            | ALLOCATABLE
                            | CODIMENSION <lbracket> <coarray-spec> <rbracket>
                            | CONTIGUOUS
                            | DIMENSION ( <component-array-spec> )
                            | POINTER
'''

import pytest
from fparser.two.Fortran2003 import Access_Spec, Dimension_Component_Attr_Spec
from fparser.two.Fortran2008 import Component_Attr_Spec, Codimension_Attr_Spec


def test_pointer_attr(f2008_create):
    '''Tests the parsing of POINTER attribute.'''
    tcls = Component_Attr_Spec
    obj = tcls('pointer')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'POINTER'


def test_allocatable_attr(f2008_create):
    '''Tests the parsing of ALLOCATABLE attribute.'''
    tcls = Component_Attr_Spec
    obj = tcls('allocatable')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'ALLOCATABLE'


def test_contiguous_attr(f2008_create):
    '''Tests the parsing of CONTIGUOUS attribute.'''
    tcls = Component_Attr_Spec
    obj = tcls('contiguous')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'CONTIGUOUS'


def test_dimension_attr(f2008_create):
    '''Tests the parsing of DIMENSION attribute.'''
    tcls = Component_Attr_Spec
    obj = tcls('dimension(a)')
    assert isinstance(obj, Dimension_Component_Attr_Spec), repr(obj)
    assert str(obj) == 'DIMENSION(a)'


def test_deferred_codimension_attr(f2008_create):
    '''Tests the parsing of CODIMENSION attribute with deferred shape.'''
    tcls = Component_Attr_Spec
    obj = tcls('codimension [:]')
    assert isinstance(obj, Codimension_Attr_Spec), repr(obj)
    assert str(obj) == 'CODIMENSION [:]'


def test_explicit_codimension_attr(f2008_create):
    '''Tests the parsing of CODIMENSION attribute with explicit shape.'''
    tcls = Component_Attr_Spec
    obj = tcls('codimension[2:*]')
    assert isinstance(obj, Codimension_Attr_Spec), repr(obj)
    assert str(obj) == 'CODIMENSION [2 : *]'


def test_explicit_list_codimension_attr(f2008_create):
    '''Tests the parsing of CODIMENSION attribute with explicit shape list.'''
    tcls = Component_Attr_Spec
    obj = tcls('codimension[1:a, b, *]')
    assert isinstance(obj, Codimension_Attr_Spec), repr(obj)
    assert str(obj) == 'CODIMENSION [1 : a, b, *]'


def test_private_attr(f2008_create):
    '''Tests the parsing of private attribute.'''
    tcls = Component_Attr_Spec
    obj = tcls('private')
    assert isinstance(obj, Access_Spec), repr(obj)
    assert str(obj) == 'PRIVATE'
