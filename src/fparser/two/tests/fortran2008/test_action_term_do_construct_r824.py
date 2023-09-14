# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------

"""Test Fortran 2008 rule R824

    action-term-do-construct is label-do-stmt
                                      do-body
                                      do-term-action-stmt

The only difference to F2003 rule R835 is that we force this rule to
use the F2008 version of label-do-stmt

"""
import pytest

from fparser.api import get_reader
from fparser.two.Fortran2008 import Action_Term_Do_Construct


@pytest.mark.usefixtures("f2008_create")
def test_concurrent():
    """Test that the Fortran2008 version supports do concurrent."""
    code = "DO 10 CONCURRENT (i = 1 : 20)\n  a(i) = 0.0\n10 b(i) = 1.0"
    reader = get_reader(code)
    obj = Action_Term_Do_Construct(reader)
    assert isinstance(obj, Action_Term_Do_Construct)
    assert str(obj) == code


def test_functional(f2008_parser):
    """The 2008 version of the Action_Term_Do_Construct class is only
    added to make sure that that a labelled do concurrent (where the
    label is not attached to a continue) is parsed in f2008. Therefore
    add a functional test to make sure this class does its job.

    """
    code = (
        "PROGRAM test\n"
        "  INTEGER :: i\n"
        "  REAL :: a(20), b(20)\n"
        "  DO 10 CONCURRENT (i = 1 : 20)\n"
        "    a(i) = 0.0\n"
        "10 b(i) = 1.0\n"
        "END PROGRAM"
    )
    tree = f2008_parser(get_reader(code))
    assert str(tree) == code
