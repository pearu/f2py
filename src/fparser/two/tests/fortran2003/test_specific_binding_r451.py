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

'''
Test Fortran 2003 rule R451 : This file tests the support for specific
binding and passed-object dummy argument within the type-bound procedure
part of a derived type.
'''

import pytest
from fparser.two.utils import NoMatchError
from fparser.two.Fortran2003 import Specific_Binding, Binding_PASS_Arg_Name


def test_valid(f2003_create):
    '''Test that valid Specific_Binding statements are parsed
    correctly.

    '''

    # simple statement
    obj = Specific_Binding('procedure sub')
    assert str(obj) == "PROCEDURE sub"

    # this should be valid
    # simple statement spaces
    #obj = Specific_Binding('  procedure  sub  ')
    #assert str(obj) == "PROCEDURE sub"

    # multi-case procedure keyword
    obj = Specific_Binding('PrOcEdUrE sub')
    assert str(obj) == "PROCEDURE sub"

    # simple statement with ::
    obj = Specific_Binding('procedure :: sub')
    assert str(obj) == "PROCEDURE :: sub"

    # simple statement with :: no space
    obj = Specific_Binding('procedure::sub')
    assert str(obj) == "PROCEDURE :: sub"

    # statement with procedure name
    obj = Specific_Binding('procedure :: sub => boat')
    assert str(obj) == "PROCEDURE :: sub => boat"

    # statement with procedure name no space
    obj = Specific_Binding('procedure::sub=>boat')
    assert str(obj) == "PROCEDURE :: sub => boat"

    # statement with interface name
    obj = Specific_Binding('procedure ( interface ) sub')
    assert str(obj) == "PROCEDURE(interface) sub"

    # statement with interface name no space
    obj = Specific_Binding('procedure(interface)::sub')
    assert str(obj) == "PROCEDURE(interface) :: sub"

    # statement with binding list
    obj = Specific_Binding('procedure , PASS :: sub')
    assert str(obj) == "PROCEDURE, PASS :: sub"

    # statement with binding list no space
    obj = Specific_Binding('procedure,PASS::sub')
    assert str(obj) == "PROCEDURE, PASS :: sub"

    # statement with multi-binding list
    obj = Specific_Binding('procedure , PASS , PUBLIC, deferred :: sub')
    assert str(obj) == "PROCEDURE, PASS, PUBLIC, DEFERRED :: sub"

    # statement with multi-binding list no space
    obj = Specific_Binding('procedure,PASS,PUBLIC,DEFERRED::sub')
    assert str(obj) == "PROCEDURE, PASS, PUBLIC, DEFERRED :: sub"

    # statement with interface name and binding list
    obj = Specific_Binding('procedure ( interface ) , PASS :: sub')
    assert str(obj) == "PROCEDURE(interface), PASS :: sub"

    # statement with interface name and binding list no space
    obj = Specific_Binding('procedure(interface),PASS::sub')
    assert str(obj) == "PROCEDURE(interface), PASS :: sub"

    # statement with binding list and procedure name
    obj = Specific_Binding('procedure , PASS :: sub => boat')
    assert str(obj) == "PROCEDURE, PASS :: sub => boat"

    # statement with binding list and procedure name no space
    obj = Specific_Binding('procedure,PASS::sub=>boat')
    assert str(obj) == "PROCEDURE, PASS :: sub => boat"


def test_invalid(f2003_create):
    ''' Test that parsing invalid Fortran syntax for
    Specific_Binding statements raises an appropriate error. '''

    # includes tests for breaking C456 and C457 
    for string in ["",
                   "  ",
                   "procedure  ",
                   "procedure ::  ",
                   "procedur sub",
                   "proceduresub",
                   "procedure : sub",
                   "procedure sub => boat",  # C456
                   "procedure :: sub =>",
                   "procedure (interface sub",
                   "procedure interface) sub",
                   "procedure (interface) :: sub => boat",  # C457
                   "procedure pass sub",
                   "procedure, pass sub",
                   "procedure, pass public sub",
                   "procedure pass, public sub",
                   "procedure pass :: sub",
                   "procedure sub sub2",
                   "procedure :: sub = boat",
                   "procedure :: sub > boat",
                   "procedure :: sub => boat boat2"]:
        if string in ["proceduresub", "procedure (interface) :: sub => boat",
                      "procedure pass :: sub"]:
            # no error is raised but should be
            continue
        print string
        with pytest.raises(NoMatchError) as excinfo:
            obj = Specific_Binding(string)
        assert "Specific_Binding: '{0}'".format(string) in str(excinfo.value)

    
def test_binding_pass_arg_name(f2003_create):
    ''' Test that Binding_PASS_Arg_Name is parsed correctly. '''

    testcls = Binding_PASS_Arg_Name
    parsecls = Specific_Binding

    aobj = testcls("pass(name)")
    assert isinstance(aobj, testcls), repr(aobj)
    assert repr(aobj) == ("Binding_PASS_Arg_Name('PASS', Name('name'))")

    line = ("procedure(analytic_function), deferred, pass(self) :: "
            "function_name")
    aobj = parsecls(line).items[1].items[1]
    assert isinstance(aobj, testcls), repr(aobj)
    assert repr(aobj) == "Binding_PASS_Arg_Name('PASS', Name('self'))"

    line = SOURCE.split('\n')[14].strip()
    obj = parsecls(line)
    assert isinstance(obj, parsecls), repr(obj)
    assert (str(obj) == "PROCEDURE, PUBLIC, PASS(self) :: "
            "abstract_method => implementation_method")
    assert repr(obj) == (
        "Specific_Binding(None, "
        "Binding_Attr_List(',', (Access_Spec('PUBLIC'), "
        "Binding_PASS_Arg_Name('PASS', Name('self')))), "
        "'::', Name('abstract_method'), Name('implementation_method'))")
