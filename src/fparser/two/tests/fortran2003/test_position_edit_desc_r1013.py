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

'''Test Fortran 2003 rule R1013 : This file tests support for the
Position_Edit_Desc class.

'''

import pytest
from fparser.two.utils import NoMatchError
from fparser.two.Fortran2003 import Position_Edit_Desc


def test_invalid_descriptor():
    '''Test invalid options raise an exception.'''

    for descriptor in ["", "  ", "1T", "XT", "T", "TL", "TR", "XT1", "XX",
                       "X X", "1XX"]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Position_Edit_Desc(descriptor)
        assert "Position_Edit_Desc: '{0}'".format(descriptor) \
            in str(excinfo.value)


def test_valid_t_descriptor(f2003_create):
    '''Test valid NT, NTL and NTR inputs provide the expected output'''

    for name in ["T", "TL", "TR"]:
        for descriptor in ["{0}1".format(name),
                           "  {0}  1  ".format(name),
                           "{0}999".format(name),
                           "{0}999".format(name.lower())]:
            result = Position_Edit_Desc(descriptor)
            assert str(result) == "".join(descriptor.split()).upper()


def test_valid_x_descriptor(f2003_create):
    '''Test valid NX inputs provide the expected output'''

    for descriptor in ["1X", "  1  X  ", "999X", "999x"]:
        result = Position_Edit_Desc(descriptor)
        assert str(result) == "".join(descriptor.split()).upper()


def test_invalid_x_descriptor_noextension(f2003_create, monkeypatch):
    '''Test that the X extension raises an exception if it is not named as
    a valid extensions.

    '''

    import fparser.two.utils as utils
    monkeypatch.setattr(utils, "EXTENSIONS", [])
    for descriptor in ["X", "  X  ", "x"]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Position_Edit_Desc(descriptor)
        assert "Position_Edit_Desc: '{0}'".format(descriptor) \
            in str(excinfo.value)


def test_valid_x_descriptor_extension(f2003_create, monkeypatch):
    '''Test that the X extension produces the expected output if it is
    named as a valid extension.

    '''

    import fparser.two.utils as utils
    monkeypatch.setattr(utils, "EXTENSIONS", ["x-format"])
    for descriptor in ["X", "  X  ", "x"]:
        result = Position_Edit_Desc(descriptor)
        assert str(result) == "X"
