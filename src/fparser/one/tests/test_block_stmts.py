# -*- coding: utf-8 -*-
# Copyright (c) 2018 Science and Technology Facilities Council.
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
File containing tests for the one.block_statements module.
'''
from __future__ import absolute_import


def test_get_type_by_name():
    ''' Tests for HasImplicitStmt.get_type_by_name(). '''
    from fparser.common.readfortran import FortranStringReader
    from fparser.common.sourceinfo import FortranFormat
    from fparser.one.typedecl_statements import Real, Integer
    from fparser.one.parsefortran import FortranParser
    # We can't just create a HasImplicitStmt object so we get the parser
    # to create a module object as that sub-classes HasImplicitStmt (amongst
    # other things).
    string = '''\
module some_block
end module some_block
'''
    reader = FortranStringReader(string)
    reader.set_format(FortranFormat(True, False))
    parser = FortranParser(reader)
    parser.parse()
    mod = parser.block.content[0]
    # Now we have a Module object, we can call get_type_by_name()...
    rtype = mod.get_type_by_name("a_real")
    assert isinstance(rtype, Real)
    itype = mod.get_type_by_name("i_int")
    assert isinstance(itype, Integer)
