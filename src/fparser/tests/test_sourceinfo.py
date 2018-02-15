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

import os
import tempfile
import pytest

from fparser.sourceinfo import FortranFormat, \
                               get_source_info_str, get_source_info


##############################################################################

@pytest.fixture(scope="module",
                params=[(None, FortranFormat(True, True)),
                        ("! -*- fortran -*-", FortranFormat(False, True)),
                        ("! -*- f77 -*-", FortranFormat(False, True)),
                        ('! -*- f90 -*-', FortranFormat(True, False)),
                        ('! -*- f03 -*-', FortranFormat(True, False)),
                        ('! -*- f08 -*-', FortranFormat(True, False)),
                        ('! -*- fix -*-', FortranFormat(False, False)),
                        ('! -*- pyf -*-', FortranFormat(True, True))])
def header(request):
    '''
    Returns parameters for header tests.
    '''
    return request.param


##############################################################################

_FIXED_SOURCE = '''      program main
      end program main
'''

_FREE_SOURCE = '''program main
end program main
'''

_FIXED_WITH_CONTINUE = '''      program main
          implicit none
          integer :: foo, &
                     bar
      end program main
'''

_FIXED_WITH_COMMENTS = '''!     The program
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
_INITIAL_TAB = "\tprogram main\n"
_MIDDLE_TAB = " \tprogram main\n"


@pytest.fixture(scope="module",
                params=[(None, FortranFormat(False, False)),
                        (_FIXED_SOURCE, FortranFormat(False, False)),
                        (_FREE_SOURCE, FortranFormat(True, False)),
                        (_FIXED_WITH_CONTINUE, FortranFormat(True, False)),
                        (_FIXED_WITH_COMMENTS, FortranFormat(False, False)),
                        (_INITIAL_TAB, FortranFormat(False, False)),
                        (_MIDDLE_TAB, FortranFormat(True, False))])
def content(request):
    '''
    Returns parameters for content tests.
    '''
    return request.param


##############################################################################

def test_get_source_info_str(header, content):
    '''
    Tests that source format is correctly identified when read from a string.
    '''
    full_source = ''
    if header[0] is not None:
        full_source += header[0] + '\n'
    if content[0] is not None:
        full_source += content[0]

    source_info = get_source_info_str(full_source)
    if header[0]:
        assert source_info == header[1]
    else:  # No header
        assert source_info == content[1]


##############################################################################

@pytest.fixture(scope="module",
                params=[('.f', None),
                        ('.f90', None),
                        ('.pyf', FortranFormat(True, True)),
                        ('.guff', None)])
def extension(request):
    '''
    Returns parameters for extension tests.
    '''
    return request.param

##############################################################################


def test_get_source_info_filename(extension, header, content):
    '''
    Tests that source format is correctly identified when read from a file.
    '''
    full_source = ''
    if header[0] is not None:
        full_source += header[0] + '\n'
    if content[0] is not None:
        full_source += content[0]

    source_file, filename = tempfile.mkstemp(suffix=extension[0], text=True)
    os.close(source_file)

    with open(filename, 'w') as source_file:
        print(full_source, file=source_file)

    try:
        source_info = get_source_info(filename)
        if extension[1] is not None:
            assert source_info == extension[1]
        elif header[0] is not None:
            assert source_info == header[1]
        else:  # No header
            assert source_info == content[1]
    except Exception as ex:
        os.remove(filename)
        raise ex

##############################################################################


def test_get_source_info_file(extension, header, content):
    '''
    Tests that source format is correctly identified when read from a file.
    '''
    full_source = ''
    if header[0] is not None:
        full_source += header[0] + '\n'
    if content[0] is not None:
        full_source += content[0]

    with tempfile.TemporaryFile(mode='w+',
                                suffix=extension[0]) as source_file:
        print(full_source, file=source_file)
        source_file.seek(0)

        source_info = get_source_info(source_file)
        if header[0] is not None:
            assert source_info == header[1]
        else:  # No header
            assert source_info == content[1]

##############################################################################
