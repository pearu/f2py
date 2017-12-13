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
Test battery associated with fparser.readfortran package.
'''
import fparser.readfortran
import fparser.tests.logging_utils
import logging
import os.path
import pytest
import re


@pytest.fixture
def log():
    logger = logging.getLogger('fparser')
    log = fparser.tests.logging_utils.CaptureLoggingHandler()
    logger.addHandler( log )
    yield log
    logger.removeHandler( log )

def test_111fortranreaderbase(log, monkeypatch):
    '''
    Tests the FortranReaderBase class.

    Currently only tests logging functionality.
    '''
    class FailFile(object):
      _buffer = ['x=1']
      def next(self):
        return self._buffer.pop()

    monkeypatch.setattr('fparser.readfortran.FortranReaderBase.id',
                        lambda x:'foo', raising=False)
    unit_under_test = fparser.readfortran.FortranReaderBase( FailFile(), True, False )
    assert str(unit_under_test.next()) == "line #1'x=1'"
    with pytest.raises(StopIteration):
        unit_under_test.next()

    assert log.messages['info']    == []
    assert log.messages['warning'] == []
    assert log.messages['error']   == []
    result = log.messages['critical'][0].split('\n')[1]
    assert result == '    1:x=1 <== while processing line'
    assert log.messages['critical'][1] == 'STOPPED READING'
    expected = 'Traceback\n'
    assert log.messages['debug'][0][:len(expected)] == expected

def test_fortranreaderbase_next_bad_include(log):
    code = "include 'nonexistant.f90'\nx=1"
    unit_under_test = fparser.readfortran.FortranStringReader( code )
    line = unit_under_test.next()
    assert str(line) == 'line #2\'x=1\''
    assert log.messages['debug'] == []
    assert log.messages['error'] == []
    assert log.messages['info'] == []
    assert log.messages['critical'] == []
    expected = '    1:include \'nonexistant.f90\' <== \'nonexistant.f90\' not found in \'.\'. INLCUDE line treated as comment line.'
    result = log.messages['warning'][0].split( '\n' )[1]
    assert result == expected

def test_fortranreaderbase_next_good_include(log):
    code = "include 'modfile.f95'\nx=2"
    includeDirectories = [os.path.dirname(__file__)]
    unit_under_test = fparser.readfortran.FortranStringReader( code,
                                                          includeDirectories )
    line = unit_under_test.next()
    assert str(line)[:19] == "Comment('! Modified" # First line of inclusion
    assert log.messages['debug'] == []
    assert log.messages['error'] == []
    assert log.messages['warning'] == []
    assert log.messages['critical'] == []
    expected = "    1:include 'modfile.f95' <== including file '{path}/modfile.f95'"
    result = log.messages['info'][0].split( '\n' )[1]
    assert result == expected.format( path=includeDirectories[0] )

def test_fortranreaderbase_info( log ):
    unit_under_test = fparser.readfortran.FortranStringReader( 'x=3' )
    thing = unit_under_test.get_source_item()
    unit_under_test.info( 'Mighty Whirlitzer', thing )
    assert log.messages['debug'] == []
    assert log.messages['error'] == []
    assert log.messages['warning'] == []
    assert log.messages['critical'] == []
    expected = '    1:x=3 <== Mighty Whirlitzer'
    result = log.messages['info'][0].split( '\n' )[1]
    assert result == expected

def test_fortranreaderbase_error( log ):
    unit_under_test = fparser.readfortran.FortranStringReader( 'x=2' )
    thing = unit_under_test.get_source_item()
    with pytest.raises(SystemExit):
        unit_under_test.error( 'Thundering Chalmer', thing )
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['warning'] == []
    assert log.messages['critical'] == []
    expected = '    1:x=2 <== Thundering Chalmer'
    result = log.messages['error'][0].split( '\n' )[1]
    assert result == expected

def test_fortranreaderbase_warning( log ):
    unit_under_test = fparser.readfortran.FortranStringReader( 'x=1' )
    thing = unit_under_test.get_source_item()
    unit_under_test.warning( 'Flatulent Hermit', thing )
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'] == []
    expected = '    1:x=1 <== Flatulent Hermit'
    result = log.messages['warning'][0].split( '\n' )[1]
    assert result == expected

def test_fortranreaderbase_handle_multilines( log ):
    code = 'character(8) :: test = \'foo"""bar'
    log.reset()
    unit_under_test = fparser.readfortran.FortranStringReader( code )
    unit_under_test.set_mode( True, True ) # Force strict free format
    unit_under_test.get_source_item()
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'] == []
    expected = 'multiline prefix contains odd number of "\'" characters'
    result = log.messages['warning'][0].split('<==')[1].lstrip()
    assert result == expected

    code = 'goo """boo\n doo""" soo \'foo'
    log.reset()
    unit_under_test = fparser.readfortran.FortranStringReader( code )
    unit_under_test.set_mode( True, True ) # Force strict free format
    unit_under_test.get_source_item()
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'] == []
    expected = 'following character continuation: "\'", expected None.'
    result = log.messages['warning'][0].split('<==')[1].lstrip()
    assert result == expected

def test_fortranreaderbase_get_source_item_fixed_nonlabel( log ):
    for i in range(1, 4):
        code = ' '*i + 'w' + ' '*(4-i) + 'integer :: i'
        log.reset()
        unit_under_test = fparser.readfortran.FortranStringReader( code )
        unit_under_test.set_mode( False, True ) # Force strict fixed format
        unit_under_test.get_source_item()
        assert log.messages['debug'] == []
        assert log.messages['info'] == []
        assert log.messages['error'] == []
        assert log.messages['critical'] == []
        result = log.messages['warning'][0].split('<==')[1].lstrip()
        expected \
      = "non-space/digit char 'w' found in column {col} of fixed Fortran code"
        assert result == expected.format( col=i+1 )

    code = 'w    integer :: i'
    log.reset()
    unit_under_test = fparser.readfortran.FortranStringReader( code )
    unit_under_test.set_mode( False, True ) # Force fixed format
    unit_under_test.get_source_item()
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'] == []
    result = log.messages['warning'][0].split('<==')[1].lstrip()
    expected = "non-space/digit char 'w' found in column 1 of fixed " \
               + "Fortran code, interpreting line as comment line"
    assert result == expected

    code = ' w   integer :: i'
    log.reset()
    unit_under_test = fparser.readfortran.FortranStringReader( code )
    unit_under_test.set_mode( False, False ) # Force sloppy fixed format
    unit_under_test.get_source_item()
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'] == []
    expected = "non-space/digit char 'w' found in column 2 " \
               + "of fixed Fortran code, switching to free format mode"
    result = log.messages['warning'][0].split('<==')[1].lstrip()
    assert result == expected

def test_fortranreaderbase_get_source_item_fixed_continuation( log ):
    code = '     character(4) :: cheese = "a & !\n     & b'
    log.reset()
    unit_under_test = fparser.readfortran.FortranStringReader( code )
    unit_under_test.set_mode( False, False) # Force sloppy fixed format
    unit_under_test.get_source_item()
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'] == []
    expected = 'following character continuation: \'"\', expected None.'
    result = log.messages['warning'][0].split('<==')[1].lstrip()
    assert result == expected

    code = '     x=1 &\n     +1 &\n     -2'
    log.reset()
    unit_under_test = fparser.readfortran.FortranStringReader( code )
    unit_under_test.set_mode( False, False ) # Force sloppy fixed format
    unit_under_test.get_source_item()
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'] == []
    expected = 'free format line continuation character `&\' detected ' \
               + 'in fix format code\n    2:     +1 &\n    3:     -2'
    result = log.messages['warning'][0].split('<==')[1].lstrip()
    assert result == expected

def test_fortranreaderbase_get_source_item_free_continuation(log):
    code = 'character(4) :: "boo & que'
    log.reset()
    unit_under_test = fparser.readfortran.FortranStringReader( code )
    unit_under_test.set_mode(True, False) # Force sloppy free format
    unit_under_test.get_source_item()
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['warning'] == []
    assert log.messages['critical'] == []
    expected = 'following character continuation: \'"\', expected None.'
    result = log.messages['error'][0].split('<==')[1].lstrip()
    assert result == expected
