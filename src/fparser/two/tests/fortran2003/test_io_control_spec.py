# Copyright (c) 2020-2021 Science and Technology Facilities Council.
#
# All rights reserved.
#
# Modifications made as part of the fparser project are distributed
# under the following license:
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
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

'''
Module containing py.test tests for the Io_Control_Spec class and
the list variant.

'''

from __future__ import print_function
import pytest
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory

# this is required to setup the fortran2003 classes
_ = ParserFactory().create(std="f2003")


def test_io_control_spec():  # R913
    ''' Test that we can construct an io-control-spec and that its str
    and repr properties are correct. '''
    tcls = Fortran2003.Io_Control_Spec
    obj = tcls('end=123')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'END = 123'
    assert repr(obj) == "Io_Control_Spec('END', Label('123'))"
    # io-control-spec only matches named entities
    with pytest.raises(Fortran2003.NoMatchError) as err:
        _ = tcls('6')
    assert "Io_Control_Spec: '6'" in str(err.value)


def test_io_control_spec_list():
    ''' Test that we correctly parse and then generate various
    forms of IO-control specification lists (R913-list). '''
    tcls = Fortran2003.Io_Control_Spec_List
    obj = tcls('23, end=123')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == '23, END = 123'
    assert (repr(obj) == "Io_Control_Spec_List(',', (Io_Control_Spec("
            "None, Int_Literal_Constant('23', None)), Io_Control_Spec('END', "
            "Label('123'))))")

    obj = tcls('123')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == '123'

    obj = tcls('123,*')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == '123, *'
    assert repr(obj) == ("Io_Control_Spec_List(',', (Io_Control_Spec("
                         "None, Int_Literal_Constant('123', None)), "
                         "Io_Control_Spec(None, Format('*'))))")

    obj = tcls('123,fmt=a')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == '123, FMT = a'
    assert repr(obj) == ("Io_Control_Spec_List(',', "
                         "(Io_Control_Spec(None, "
                         "Int_Literal_Constant('123', None)), "
                         "Io_Control_Spec('FMT', Name('a'))))")

    obj = tcls('123,nml=a')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == '123, NML = a'
    assert repr(obj) == ("Io_Control_Spec_List(',', "
                         "(Io_Control_Spec(None, "
                         "Int_Literal_Constant('123', None)), "
                         "Io_Control_Spec('NML', Name('a'))))")

    obj = tcls('123, "(I3)"')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == '123, "(I3)"'

    obj = tcls('123,a')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == '123, a'

    # Unit named and not the first argument
    obj = tcls('fmt=b, unit=123')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'FMT = b, UNIT = 123'


def test_io_control_spec_list_invalid_first_entry(monkeypatch):
    ''' Test that the special handling for the first item in the list
    rejects an invalid, unnamed entry. '''
    tcls = Fortran2003.Io_Control_Spec_List
    # Check the various things we should not match. To exercise some of them
    # the test for whether the first element in the list is a valid IO unit
    # must fail. Unfortunately, because of #276 this is currently impossible
    # and therefore we monkeypatch Io_Unit to make it appear that
    # it does not match.

    def raise_exception(_x, _y):
        raise Fortran2003.NoMatchError("monkeypatched function")
    monkeypatch.setattr(Fortran2003.Io_Unit, "__new__",
                        raise_exception)

    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls("'name'")
    assert "Io_Control_Spec_List: ''name''" in str(err.value)


def test_io_spec_list_constraints():
    ''' Check that various constraints are applied in the matching. Currently
    only C910, C916-918 are implemented (#267). '''
    tcls = Fortran2003.Io_Control_Spec_List
    # C910 - must have a unit number
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls('fmt="some-fmt", end=10')
    assert "Io_Control_Spec_List: 'fmt=\"some-fmt\", end=10'" in str(err.value)
    # Again but when list contains only one entry
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls('fmt="some-fmt"')
    assert "Io_Control_Spec_List: 'fmt=\"some-fmt\"'" in str(err.value)

    # C917, if format is second item and is unamed then first item must be
    # unnamed unit number.
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls('unit=10, "(\'a format\')"')
    assert "Io_Control_Spec_List: 'unit=10" in str(err.value)

    # C918, as for 917 but for a namelist group name
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls('unit=10, nml_grp_name')
    assert "Io_Control_Spec_List: 'unit=10" in str(err.value)

    # C916 - cannot have both a namelist and a format
    with pytest.raises(Fortran2003.NoMatchError) as err:
        tcls('123,nml=a,fmt=b')
    assert "Io_Control_Spec_List: '123,nml=a,fmt=b'" in str(err.value)
