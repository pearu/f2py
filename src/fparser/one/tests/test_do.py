# -*- coding: utf-8 -*-
# Copyright (c) 2019 Science and Technology Facilities Council.
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

from __future__ import absolute_import, print_function

import pytest

from fparser.common.utils import AnalyzeError
from fparser.common.sourceinfo import FortranFormat
from fparser.one.parsefortran import FortranParser
from fparser.common.readfortran import FortranStringReader


@pytest.fixture(scope='module', params=[None, 'loop_name'])
def name(request):
    '''
    Yields None, then a construct name string.
    '''
    yield request.param

@pytest.fixture(scope='module', params=[None, '123'])
def label(request):
    '''
    Yields None, then a label string.
    '''
    yield request.param

@pytest.fixture(scope='module', params=[True, False])
def control_comma(request):
    '''
    Yields a comma, or not.
    '''
    yield request.param

@pytest.fixture(scope='module', params=['1'])
def initial_expression(request):
    '''
    Yields a series of loop initial conditions.
    '''
    yield request.param

@pytest.fixture(scope='module',
                params=['1',
                        '10',
                        'x+y',
                        'size(array)',
                        'size(this%array)'])
def terminal_expression(request):
    '''
    Yields a series of loop terminating conditions.
    '''
    yield request.param

@pytest.fixture(scope='module', params=[None, '1'])
def incrament_expression(request):
    '''
    Yields a series of loop incrament expressions.
    '''
    yield request.param

@pytest.fixture(scope='module', params=[None, '1'])
def mask_expression(request):
    '''
    Yields a series of concurrent loop mask expressions.
    '''
    yield request.param

@pytest.fixture(scope='module', params=[None, 'loop_name'])
def end_name(request):
    '''
    Yields a series of potential end of loop names.
    '''
    yield request.param

@pytest.fixture(scope='module', params=[None, '123'])
def end_label(request):
    '''
    Yields a series of potential end of loop labels.
    '''
    yield request.param

@pytest.fixture(scope='module',
                params=[None,
                        'logical',
                        'real(r16)',
                        'integer(kind=i32)'])
def typespec(request):
    '''
    Yields a series of type specifications.
    '''
    yield request.param + ' :: ' if request.param else None

def test_do(name, label, control_comma, initial_expression,
            terminal_expression, incrament_expression, end_name, end_label):
    #pylint: disable=redefined-outer-name
    '''
    Checks that the "do" loop parser understands a form of the syntax.
    '''
    name_snippet = name + ': ' if name else None
    label_snippet = label + ' ' if label else None
    comma_snippet = ', ' if control_comma else None
    do_code = '''{name}do {label}{comma}variable = {init}, {term}{inc}
  write (6, '(I0)') variable
{endlabel} end do {endname}
'''.format(name=name_snippet or '',
           label=label_snippet or '',
           comma=comma_snippet or '',
           init=initial_expression,
           term=terminal_expression,
           inc=incrament_expression or '',
           endlabel=end_label or '',
           endname=end_name or '')
    do_expected = '''  {name}DO {label}variable = {init}, {term}{inc}
    WRITE (6, '(I0)') variable
{endlabel} END DO {endname}
'''.format(name=name_snippet or '',
           label=label_snippet or '',
           init=initial_expression,
           term=terminal_expression,
           inc=incrament_expression or '',
           endlabel=end_label or ' ',
           endname=end_name or '')
    print(do_code)
    do_reader = FortranStringReader(do_code)
    do_reader.set_format(FortranFormat(True, False))
    do_parser = FortranParser(do_reader)
    if (name != end_name) or (label and (label != end_label)):
        with pytest.raises(AnalyzeError):
            do_parser.parse()
    else:
        do_parser.parse()
        loop = do_parser.block.content[0]
        assert str(loop).splitlines() == do_expected.splitlines()

def test_do_while(name, label, control_comma, terminal_expression,
                  end_name, end_label):
    #pylint: disable=redefined-outer-name
    '''
    Checks that the "do while" loop parser understands a form of the syntax.
    '''
    name_snippet = name + ': ' if name else None
    label_snippet = label + ' ' if label else None
    comma_snippet = ', ' if control_comma else None
    code = '''{name}do {label}{comma}while ({term})
  write (6, '(I0)') variable
{endlabel} end do {endname}
'''.format(name=name_snippet or '',
           label=label_snippet or '',
           comma=comma_snippet or '',
           term=terminal_expression,
           endlabel=end_label or '',
           endname=end_name or '')
    expected = '''  {name}DO {label}while ({term})
    WRITE (6, '(I0)') variable
{endlabel} END DO {endname}
'''.format(name=name_snippet or '',
           label=label_snippet or '',
           term=terminal_expression,
           endlabel=end_label or ' ',
           endname=end_name or '')
    print(code)
    reader = FortranStringReader(code)
    reader.set_format(FortranFormat(True, False))
    parser = FortranParser(reader)
    if (name != end_name) or (label and (label != end_label)):
        with pytest.raises(AnalyzeError):
            parser.parse()
    else:
        parser.parse()
        loop = parser.block.content[0]
        assert str(loop).splitlines() == expected.splitlines()

def test_do_concurrent(name, label, control_comma, typespec,
                       initial_expression, terminal_expression, incrament_expression,
                       mask_expression, end_name, end_label):
    #pylint: disable=redefined-outer-name
    '''
    Checks that the "do concurrent" loop parser understands a form of the syntax.
    '''
    name_snippet = name + ': ' if name else None
    label_snippet = label + ' ' if label else None
    comma_snippet = ', ' if control_comma else None
    inc_snippet = ':' + incrament_expression if incrament_expression else None
    mask_snippet = ', ' + mask_expression if mask_expression else None
    code = '''{name}do {label}{comma}concurrent ({tspec}variable={init}:{term}{inc}{mask})
  write (6, '(I0)') variable
{endlabel} end do {endname}
'''.format(name=name_snippet or '',
           label=label_snippet or '',
           comma=comma_snippet or '',
           tspec=typespec or '',
           init=initial_expression,
           term=terminal_expression,
           inc=inc_snippet or '',
           mask=mask_snippet or '',
           endlabel=end_label or '',
           endname=end_name or '')
    expected = '''  {name}DO {label}concurrent ({tspec}variable={init}:{term}{inc}{mask})
    WRITE (6, '(I0)') variable
{endlabel} END DO {endname}
'''.format(name=name_snippet or '',
           label=label_snippet or '',
           tspec=typespec or '',
           init=initial_expression,
           term=terminal_expression,
           inc=inc_snippet or '',
           mask=mask_snippet or '',
           endlabel=end_label or ' ',
           endname=end_name or '')
    print(code)
    reader = FortranStringReader(code)
    reader.set_format(FortranFormat(True, False))
    parser = FortranParser(reader)
    if (name != end_name) or (label and (label != end_label)):
        with pytest.raises(AnalyzeError):
            parser.parse()
    else:
        parser.parse()
        loop = parser.block.content[0]
        assert str(loop).splitlines() == expected.splitlines()
