# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017 Science and Technology Facilities Council
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
# Modified M. Hambley, UK Met Office
##############################################################################
'''
Tests that the parser understands deferred methods.
'''
import fparser.Fortran2003
import fparser.readfortran


def test_deferred_method():
    '''
    Tests that the parser understands deferred methods.
    '''
    source = '''! Abstract type
module abstract_test

  implicit none
  private

  type, abstract, public :: test_type
  contains
    procedure(method_interface), deferred :: method
  end type test_type

end module abstract_test
'''
    expected = '''! Abstract type
MODULE abstract_test
  IMPLICIT NONE
  PRIVATE
  TYPE, ABSTRACT, PUBLIC :: test_type
    CONTAINS
    PROCEDURE(method_interface), DEFERRED :: method
  END TYPE test_type
END MODULE abstract_test
'''.strip().split('\n')

    reader = fparser.readfortran.FortranStringReader(source)
    program_unit = fparser.Fortran2003.Program(reader)
    result = str(program_unit).strip().split('\n')
    assert result == expected
