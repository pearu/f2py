# Copyright (c) 2019 Science and Technology Facilities Council

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

'''Test Fortran 2003 rule R1002 : This file tests the support for a
format specification.

'''

import pytest
from fparser.two.Fortran2003 import Format_Specification
from fparser.two.utils import NoMatchError, InternalError


def test_format_specification_single(f2003_create):
    '''Check that some basic format specifications with single format
    items are parsed correctly. Individual items are tested by the
    associated classes.

    '''
    # 1: data-edit-desc, 2: r data-edit-desc, 3: control-edit-desc, 4:
    # char-string-edit-desc, 5: format-item-list
    for myinput in ["()", "(E2.2)", "(2E2.2)", "(/)", "('hello')",
                    "(2(E2.2))"]:
        ast = Format_Specification(myinput)
        assert myinput in str(ast)


def test_format_specification_multi(f2003_create):
    '''Check that a basic format specification with multiple format items
    are parsed correctly. Individual format items are tested by the
    associated classes.

    '''
    # 1: data-edit-desc, 2: r data-edit-desc, 3: control-edit-desc, 4:
    # char-string-edit-desc, 5: format-item-list
    my_input = "(E2.2, 2E2.2, /, 'hello', 2(E2.2, 'there'))"
    ast = Format_Specification(my_input)
    assert my_input in str(ast)


def test_format_specification_spaces(f2003_create):
    '''Check that a basic format specification with multiple format items
    are parsed correctly with spaces. Individual format items are
    tested by the associated classes.

    '''
    # spaces before and after the brackets
    my_input = ("  (  )  ")
    ast = Format_Specification(my_input)
    # 1: data-edit-desc, 2: r data-edit-desc, 3: control-edit-desc, 4:
    # char-string-edit-desc, 5: format-item-list
    my_input = ("(  E2.2  ,  2E2.2  ,  /  ,  'hello'  ,   "
                "2(E2.2, 'there')  )")
    ast = Format_Specification(my_input)
    assert "(E2.2, 2E2.2, /, 'hello', 2(E2.2, 'there'))" in str(ast)


def test_format_specification_hollerith(f2003_create):
    '''Check that a basic format specification with a hollerith string
    item is parsed correctly. Individual format items are tested by
    the associated classes.

    '''
    my_input = ("(2H,,)")
    ast = Format_Specification(my_input)


def test_format_specification_C1002(f2003_create):
    '''Check that basic format specifications conforming to the C1002
    constraints are parsed correctly. This test actually checks class
    Format_Item_C1002 as the constraints have been implemented as a
    different class.

    '''
    # no comma needed between a P edit descriptor and an immediately
    # following F, E, EN, ES, D, or G edit descriptor, possibly
    # preceded by a repeat specifier
    for my_input in ["(2P, 2E2.2)", "(2P 2E2.2)", "(2P2E2.2)"]:
        ast = Format_Specification(my_input)
    **********************************


def test_syntaxerror(f2003_create):
    ''' XXX '''
    for my_input in [None, "", "  ", "(", ")"]:
        with pytest.raises(NoMatchError):
            _ = Format_Specification(my_input)

    
