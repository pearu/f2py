# Copyright (c) 2022 Science and Technology Facilities Council

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

"""Test Fortran 2008 rule R904

   open-stmt is OPEN ( connect-spec-list )

   R905 connect-spec is [ UNIT = ] file-unit-number
                    or ACCESS = scalar-default-char-expr
                    or ACTION = scalar-default-char-expr
                    or ASYNCHRONOUS = scalar-default-char-expr
                    or BLANK = scalar-default-char-expr
                    or DECIMAL = scalar-default-char-expr
                    or DELIM = scalar-default-char-expr
                    or ENCODING = scalar-default-char-expr
                    or ERR = label
                    or FILE = file-name-expr
                    or FORM = scalar-default-char-expr
                    or IOMSG = iomsg-variable
                    or IOSTAT = scalar-int-variable
                    or NEWUNIT = scalar-int-variable
                    or PAD = scalar-default-char-expr
                    or POSITION = scalar-default-char-expr
                    or RECL = scalar-int-expr
                    or ROUND = scalar-default-char-expr
                    or SIGN = scalar-default-char-expr
                    or STATUS = scalar-default-char-expr

R906 file-name-expr is scalar-d

"""

import pytest
from fparser.api import get_reader, walk
from fparser.two import Fortran2008
from fparser.two.utils import NoMatchError


def test_open_newunit(f2008_parser):
    """Test previous subclasses are still matched correctly in a subroutine."""
    tree = f2008_parser(
        get_reader(
            """\
subroutine myopen( unit, file )
  integer, intent(out):: unit
  character(len=*), intent(in):: file
  open( newunit=unit, file=file )
endsubroutine myopen
    """
        )
    )
    assert walk(tree, Fortran2008.Open_Stmt)
    assert "STOP" in str(tree)
