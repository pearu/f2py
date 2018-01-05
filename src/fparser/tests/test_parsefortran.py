#!/usr/bin/env python
# -*- coding: utf-8 -*-
##############################################################################
# Modified work Copyright (c) 2017 Science and Technology Facilities Council
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
'''
Tests the fparser.parsefortran module.
'''

import pytest
from fparser.tests.logging_utils import log
import fparser.parsefortran
import fparser.readfortran


def test_log_empty(log):
    '''
    Tests that a reader without next() method causes an event to be logged.
    '''
    class EmptyReader(object):
        '''
        A faux reader with no next() method.
        '''
        id = 'thingumy'

    unit_under_test = fparser.parsefortran.FortranParser(EmptyReader())
    unit_under_test.analyze()
    assert log.messages == {'debug':    [],
                            'info':     ['Nothing to analyze.'],
                            'warning':  [],
                            'error':    [],
                            'critical': []}


def test_log_cache(log):
    '''
    Tests that using a cached reader object logs an event.
    '''
    class Readerlike(object):
        '''
        Dummy reader class, the only purpose of which is to have an id and not
        cause the parser to fail.
        '''
        id = 'thisun'

        def next(self):
            '''
            Simple non-failure-causing method.
            '''
            yield 'NOT A THING'
            raise StopIteration

    # Expect everything to go okay, no log messages.
    log.reset()
    __ = fparser.parsefortran.FortranParser(Readerlike())
    assert log.messages == {'debug':    [],
                            'info':     [],
                            'warning':  [],
                            'error':    [],
                            'critical': []}

    # This time we should use a cached log.
    __ = fparser.parsefortran.FortranParser(Readerlike())
    assert log.messages == {'debug':    [],
                            'info':     ['using cached thisun'],
                            'warning':  [],
                            'error':    [],
                            'critical': []}


def test_log_failure(log, monkeypatch):
    '''
    Tests that an unexpected read failure causes an event to be logged.
    '''
    def faulty_next(self, ignore_comments=False):
        '''
        Raies any old exception.
        '''
        raise Exception('That''s all folks!')

    monkeypatch.setattr('fparser.readfortran.FortranStringReader.next',
                        faulty_next)
    reader = fparser.readfortran.FortranStringReader('')
    unit_under_test = fparser.parsefortran.FortranParser(reader)
    with pytest.raises(Exception):
        unit_under_test.parse()
    assert log.messages['debug'][0].startswith('Traceback\n')
    assert log.messages['info'] == []
    assert log.messages['warning'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'][0].startswith('While processing')
    assert log.messages['critical'][1] == 'STOPPED PARSING'
