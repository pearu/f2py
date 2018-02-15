# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017-2018 Science and Technology Facilities Council
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
##############################################################################
# Modified M.Hambley, UK Met Office
##############################################################################
'''
Test battery associated with fparser.sourceinfo package.
'''
from __future__ import print_function

import pytest
import fparser.sourceinfo

@pytest.fixture(scope="module",
                params=[('', True, True),
                        ("-*- fortran -*-", False, True),
                        ("-*- f77 -*-",     False, True),
                        ('-*- f90 -*-',     True,  False),
                        ('-*- f03 -*-',     True,  False),
                        ('-*- f08 -*-',     True,  False),
                        ('-*- fix -*-',     False, False),
                        ('-*- pyf -*-',     True,  True)])
def header(request):
    return request.param

_fixed_source = '''      program main
      end program main
'''

_free_source = '''program main
end program main
'''

_fixed_with_continuation = '''      program main
          implicit none
          integer :: foo, &
                     bar
      end program main
'''

_fixed_with_comments = '''!     The program
      program main
c         Enforce explicit variable declaration
          implicit none
*         variables
          integer :: foo
      end program main
'''

# Tabs are not actually in the Fortran character set but fparser has handled
# them in the past even if it shouldn't have. We have to continue handling
# them for the time being until we work out why they were supported.
#
_initial_tab = "\tprogram main\n"
_middle_tab = " \tprogram main\n"

@pytest.fixture(scope="module",
                params=[(_fixed_source,            False, False),
                        (_free_source,             True,  False),
                        (_fixed_with_continuation, True,  False),
                        (_fixed_with_comments,     False, False),
                        (_initial_tab,             False, False),
                        (_middle_tab,              True,  False)])
def content(request):
    return request.param

def test_get_source_info_str(header, content):
    full_source = header[0] + '\n' + content[0]
    fixed, strict = fparser.sourceinfo.get_source_info_str(full_source)
    if header[0]:
      assert fixed  == header[1]
      assert strict == header[2]
    else: # No header
      assert fixed  == content[1]
      assert strict == content[2]
