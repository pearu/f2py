# Copyright (c) 2024 Science and Technology Facilities Council

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

"""Test Fortran 2008 rule R856

    error-stop-stmt is ERROR STOP [ stop-code ]
"""

import pytest
from fparser.two.utils import NoMatchError
from fparser.two.Fortran2008 import Stop_Code


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize("string", ["1", "- 1", '"abc"', "'abc'", "'abc' // 'def'"])
def test_simple_stop_code(string):
    """Test that error-stop matches the expected valid values."""
    result = Stop_Code(string)
    assert str(result) == string


@pytest.mark.usefixtures("f2008_create")
@pytest.mark.parametrize("string", ["call sub()", "do i", "1, 2, 3"])
def test_simple_stop_code_errors(string):
    """Test that invalid stop codes are handled."""
    with pytest.raises(NoMatchError) as err:
        Stop_Code(string)
    assert f"Stop_Code: '{string}'" in str(err.value)
