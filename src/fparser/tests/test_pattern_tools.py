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
# Modified M.Hambley, UK Met Office
##############################################################################
'''
Test battery associated with fparser.pattern_tools package.
'''

import re
import fparser.pattern_tools

def test_name_pattern():
    assert fparser.pattern_tools.name.match('a1_a')
    assert abs(fparser.pattern_tools.name).match('a1_a')
    assert not abs(fparser.pattern_tools.name).match('a1_a[]')

def test_kind_param_pattern():
    m = abs(fparser.pattern_tools.kind_param)
    assert m.match('23')
    assert m.match('SHORT')

def test_signed_digit_string_pattern():
    m = abs(fparser.pattern_tools.signed_digit_string)
    assert m.match('23')
    assert m.match('+ 23')
    assert m.match('- 23')
    assert m.match('-23')
    assert not m.match('+n')

def test_thing():
    m = ~fparser.pattern_tools.sign.named() \
        + fparser.pattern_tools.digit_string.named('number')
    r = m.match('23')
    assert r.groupdict()=={'number': '23', 'sign': None}
    r = m.match('- 23')
    assert r.groupdict()=={'number': '23', 'sign': '-'}

def test_char_literal_constant_pattern():
    m = abs(fparser.pattern_tools.char_literal_constant)
    assert m.match('"adadfa"')
    assert m.match('"adadfa""adad"')
    assert m.match('HEY_"adadfa"')
    assert m.match('HEY _ "ad\tadfa"')
    assert not m.match('adadfa')

def test_multi_op_pattern():
    m = fparser.pattern_tools.mult_op.named()
    assert m.rsplit('a *  b')
    assert m.lsplit('a * c* b') == ('a','*','c* b')
    assert m.rsplit('a * c* b') == ('a * c','*','b')
    assert m.lsplit('a * b ** c') == ('a','*','b ** c')
    assert m.rsplit('a * b ** c') == ('a','*','b ** c')
    assert m.lsplit('a * b ** c * d') == ('a','*','b ** c * d')
    assert m.rsplit('a * b ** c * d') == ('a * b ** c','*','d')

def test_power_op_pattern():
    m = fparser.pattern_tools.power_op.named()
    assert m.rsplit('a **  b')
    assert m.lsplit('a * b ** c') == ('a * b','**','c')
    assert m.rsplit('a * b ** c') == ('a * b','**','c')
    assert m.lsplit('a ** b ** c') == ('a','**','b ** c')
    assert m.rsplit('a ** b ** c') == ('a ** b','**','c')
