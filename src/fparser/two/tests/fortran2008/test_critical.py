# Copyright (c) 2018-2021 Science and Technology Facilities Council.

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


from fparser.api import get_reader
from fparser.two.utils import FortranSyntaxError
from fparser.two.Fortran2008 import Critical_Construct

import pytest


def test_critical(f2008_create):
    critical = Critical_Construct(
        get_reader(
            """\
            critical
               a = 1 + b
            end critical
            """
        )
    )

    assert "CRITICAL\n  a = 1 + b\nEND CRITICAL" in str(critical)


def test_named_critical(f2008_create):
    critical = Critical_Construct(
        get_reader(
            """\
            foo: critical
               a = 1 + b
            end critical foo
            """
        )
    )

    assert "foo:CRITICAL\n  a = 1 + b\nEND CRITICAL foo" in str(critical)


def test_end_critical_missing_start_name(f2008_create):  # C809
    with pytest.raises(FortranSyntaxError):
        Critical_Construct(
            get_reader(
                """\
                critical
                end critical foo
                """
            )
        )


def test_end_critical_missing_end_name(f2008_create):  # C809
    with pytest.raises(FortranSyntaxError):
        Critical_Construct(
            get_reader(
                """\
                foo: critical
                end critical
                """
            )
        )


def test_end_critical_wrong_name(f2008_create):  # C809
    with pytest.raises(FortranSyntaxError):
        Critical_Construct(
            get_reader(
                """\
                foo: critical
                end critical bar
                """
            )
        )
