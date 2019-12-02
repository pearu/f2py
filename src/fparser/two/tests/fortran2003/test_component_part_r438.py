# Copyright (c) 2019 Science and Technology Facilities Council.

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

'''Test Fortran 2003 rule R438: This file tests the support for the components
of a derived type.

'''

import pytest
from fparser.common.sourceinfo import FortranFormat
from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Component_Part
from fparser.two.utils import NoMatchError


@pytest.mark.parametrize("var_type", ["integer",
                                      "logical",
                                      "character(len=1)",
                                      "real*8",
                                      "real(r_def)"])
def test_data_component_part(f2003_create, var_type):
    ''' Test that various data component declarations are
    recognised. R440. '''
    code = var_type + " :: iflag"
    reader = FortranStringReader(code, ignore_comments=False)
    # Ensure reader in in 'free-format' mode
    reader.set_format(FortranFormat(True, False))
    obj = Component_Part(reader)
    assert "iflag" in str(obj)


@pytest.mark.parametrize("interface, attributes",
                         [("", "pointer, nopass"),
                          ("real_func", "pointer"),
                          ("real_func", "pointer, pass")])
def test_proc_component_part(f2003_create, interface, attributes):
    ''' Test that various procedure component declarations are
    recognised. R445. '''
    code = "procedure({0}), {1} :: my_proc".format(interface, attributes)
    reader = FortranStringReader(code, ignore_comments=False)
    # Ensure reader in in 'free-format' mode
    reader.set_format(FortranFormat(True, False))
    obj = Component_Part(reader)


@pytest.mark.parametrize("attributes", ["nopass", "pass"])
def test_proc_component_pointer(f2003_create, attributes):
    ''' R445, C449: POINTER shall appear in each
    proc-component-attr-spec-list. '''
    code = "procedure(), {0} :: my_proc".format(attributes)
    reader = FortranStringReader(code, ignore_comments=False)
    # Ensure reader in in 'free-format' mode
    reader.set_format(FortranFormat(True, False))
    with pytest.raises(NoMatchError):
        _ = Component_Part(reader)
