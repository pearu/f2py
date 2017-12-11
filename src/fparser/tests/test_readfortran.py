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
import pytest


def test_fortranreaderbase(monkeypatch):
    '''
    Tests the FortranReaderBase class.

    Currently only tests logging functionality.
    '''
    logger = logging.getLogger('fparser')
    log = fparser.tests.logging_utils.CaptureLoggingHandler()
    logger.addHandler(log)

    class BrokenFile(object):
        '''
        A file-like object which unexpectedly fails.
        '''
        def next(self):
            '''
            Failing method.
            '''
            raise Exception('None shall pass')

    monkeypatch.setattr(fparser.readfortran.FortranReaderBase,
                        'id', lambda x: None, raising=False)
    unit_under_test = fparser.readfortran.FortranReaderBase(BrokenFile(),
                                                            True, True)
    with pytest.raises(StopIteration):
        unit_under_test.next()

    assert 'STOPPED READING' in log.messages['critical']

@pytest.fixture
def log():
    logger = logging.getLogger('fparser')
    log = fparser.tests.logging_utils.CaptureLoggingHandler()
    logger.addHandler( log )
    yield log
    logger.removeHandler( log )

def test_fortranreaderbase_next(log):
    code = 'include nonexistant.f90'
    unit_under_test = fparser.readfortran.FortranReaderBase( code, True, True )
    expected = 'nonexistant.f90 not found in foo. INLCUDE line treated as comment line.'
    assert log.messages == {'debug':    [],
                           'info':     [],
                           'warning':  [expected],
                           'error':    [],
                           'critical': []}

    log.reset()
    code = 'include modfile.f95'
    unit_under_test = fparser.readfortran.FortranReaderBase( code, True, True )
    expected ='including file modfile.f95'
    assert log.messages == {'debug':    [],
                           'info':     [expected],
                           'warning':  [],
                           'error':    [],
                           'critical': []}

    class FaultyFile:
      def next():
        raise Exception('Stop! Hammer time!')

    log.reset()
    unit_under_test = fparser.readfortran.FortranReaderBase( 'x=1',
                                                             True, True )
    critical = ['FATAL ERROR while processing line', 'STOPPED READING']
    debug = ['Traceback\n']
    assert log.messages == {'debug':    debug,
                           'info':     [],
                           'warning':  [],
                           'error':    [],
                           'critical': critical}

def test_fortranreaderbase_info( log ):
    unit_under_test = fparser.readfortran.FortranReaderBase( 'x=1',
                                                             True, True )
    unit_under_test.info( 'Mighty Whirlitzer' )
    assert log.messages == {'debug':    [],
                           'info':     ['Mighty foo'],
                           'warning':  [],
                           'error':    [],
                           'critical': []}

def test_fortranreaderbase_error( log ):
    unit_under_test = fparser.readfortran.FortranReaderBase( 'x=1',
                                                             True, True )
    unit_under_test.error( 'Thundering Chalmer' )
    assert log.messages == {'debug':    [],
                           'info':     [],
                           'warning':  [],
                           'error':    ['Thundering foo'],
                           'critical': []}

def test_fortranreaderbase_warning( log ):
    unit_under_test = fparser.readfortran.FortranReaderBase( 'x=1',
                                                             True, True )
    unit_under_test.info( 'Flatulent Hermit' )
    assert log.messages == {'debug':    [],
                           'info':     [],
                           'warning':  ['Flatulent foo'],
                           'error':    [],
                           'critical': []}

def test_fortranreaderbase_handle_multilines( log ):
    code = '" """x=1\ny=2"""'
    unit_under_test = fparser.readfortran.FortranReaderBase( code,
                                                             True, True )
    expected = 'multiline prefix contains odd number of foo characters'
    assert log.messages == {'debug':    [],
                           'info':     [],
                           'warning':  [expected],
                           'error':    [],
                           'critical': []}

    log.reset()
    code = '"""boo &\n & goo"""'
    unit_under_test = fparser.readfortran.FortranReaderBase( code,
                                                             True, True )
    expected = 'ASSERTION FAILURE(pyf)', \
               'following character continuation: foo, expected None.'
    assert log.messages == {'debug':    [],
                           'info':     [],
                           'warning':  [expected],
                           'error':    [],
                           'critical': []}

def test_fortranreaderbase_get_source_item( log ):
    code = 'w'
    unit_under_test = fparser.readfortran.FortranReaderBase( code,
                                                             False, True )
    expected = 'non-space/digit char foo found in column 0', \
               ' of fixed Fortran code', \
               ', interpreting line as comment line'
    assert log.messages == {'debug':    [],
                           'info':     [],
                           'warning':  [expected],
                           'error':    [],
                           'critical': []}

    log.reset()
    unit_under_test = fparser.readfortran.FortranReaderBase( code,
                                                             False, False )
    expected = 'non-space/digit char foo found in column 0', \
               ' of fixed Fortran code', \
               ', interpreting line as comment line', \
               ', switching to free format mode'
    assert log.messages == {'debug':    [],
                           'info':     [],
                           'warning':  [expected],
                           'error':    [],
                           'critical': []}

    log.reset()
    code = ''
    unit_under_test = fparser.readfortran.FortranReaderBase( code,
                                                             False, False )
    expected = 'ASSERTION FAILURE(fix)', \
               'following character continuation: foo, expected None.'
    assert log.messages == {'debug':    [],
                           'info':     [],
                           'warning':  [expected],
                           'error':    [],
                           'critical': []}

    log.reset()
    code = 'x=1 &\n  +1'
    unit_under_test = fparser.readfortran.FortranReaderBase( code,
                                                             False, True )
    expected = 'free format line continuation character `&\' detected' \
               ' in fix format code'
    assert log.messages == {'debug':    [],
                           'info':     [],
                           'warning':  [expected],
                           'error':    [],
                           'critical': []}

    log.reset()
    code = '"""boo & que'
    unit_under_test = fparser.readfortran.FortranReaderBase( code,
                                                             True, True )
    expected = 'ASSERTION FAILURE(free)', \
               'following character continuation: foo, expected None.'
    assert log.messages == {'debug':    [],
                           'info':     [],
                           'warning':  [],
                           'error':    [expected],
                           'critical': []}
