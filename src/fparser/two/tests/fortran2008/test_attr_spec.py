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

'''Test Fortran 2008 rule R502.

    <attr-spec> = <access-spec>
                  | ALLOCATABLE
                  | ASYNCHRONOUS
                  | CODIMENSION <lbracket> <coarray-spec> <rbracket>
                  | CONTIGUOUS
                  | DIMENSION ( <component-array-spec> )
                  | EXTERNAL
                  | INTENT (<intent-spec>)
                  | INTRINSIC
                  | <language-binding-spec>
                  | OPTIONAL
                  | PARAMETER
                  | POINTER
                  | PROTECTED
                  | SAVE
                  | TARGET
                  | VALUE
                  | VOLATILE
'''

import pytest
from fparser.two.Fortran2008 import (
    Attr_Spec, Codimension_Attr_Spec, Deferred_Coshape_Spec, Coshape_Spec)
from fparser.two import Fortran2003


def test_attr_spec(f2008_create):
    '''Tests the parsing of common attributes.'''
    tcls = Attr_Spec
    attrs = {
        'allocatable': 'ALLOCATABLE', 'asynchronous': 'ASYNCHRONOUS',
        'contiguous': 'CONTIGUOUS',
        'external': 'EXTERNAL', 'intrinsic': 'INTRINSIC',
        'optional': 'OPTIONAL', 'parameter': 'PARAMETER', 'pointer': 'POINTER',
        'protected': 'PROTECTED', 'save': 'SAVE', 'target': 'TARGET',
        'value': 'VALUE', 'volatile': 'VOLATILE'}
    for attr, ref in attrs.items():
        obj = tcls(attr)
        assert isinstance(obj, tcls), repr(obj)
        assert str(obj) == ref

    obj = tcls('dimension(:)')
    assert isinstance(obj, Fortran2003.Dimension_Attr_Spec), repr(obj)
    assert str(obj) == 'DIMENSION(:)'

    obj = tcls('codimension [*]')
    assert isinstance(obj, Codimension_Attr_Spec), repr(obj)
    assert str(obj) == 'CODIMENSION [*]'


def test_codimension_attr_spec(f2008_create):
    '''Tests the parsing of codimension attributes.'''
    tcls = Codimension_Attr_Spec
    attrs = {
        'codimension[*]': 'CODIMENSION [*]',
        'codimension [:]': 'CODIMENSION [:]',
        'codimension [1:5, 2, 3:*]': 'CODIMENSION [1 : 5, 2, 3 : *]'}
    for attr, ref in attrs.items():
        obj = tcls(attr)
        assert isinstance(obj, tcls), repr(obj)
        assert str(obj) == ref


def test_invalid_coshape_spec(f2008_create):
    '''Tests invalid codimension attributes.'''
    tcls = Codimension_Attr_Spec
    attrs = ['codimension[1:5]', 'codimension[3:]', 'codimension[:5]',
             'codimension[,,]', 'codimension[1, 5, 3 *]']
    for attr in attrs:
        with pytest.raises(Fortran2003.NoMatchError):
            _ = tcls(attr)

    with pytest.raises(Fortran2003.NoMatchError):
        _ = Deferred_Coshape_Spec('')

    attrs = [':', 'a:', ':b']
    for attr in attrs:
        with pytest.raises(Fortran2003.NoMatchError):
            _ = Coshape_Spec(attr)
