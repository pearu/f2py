# Modified work Copyright (c) 2017 Science and Technology Facilities Council
# Original work Copyright (c) 1999-2008 Pearu Peterson

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

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

'''
Provides functions to determine whether a piece of Fortran source is free or
fixed format. It also tries to differentiate between strict and "pyf" although
I'm not sure what that is.
'''

import re
import os


##############################################################################

class FortranFormat(object):
    '''
    Describes the nature of a piece of Fortran source.

    Source can be fixed or free format. It can also be "strict" or
    "not strict" although it's not entirely clear what that means. It may
    refer to the strictness of adherance to fixed format although what that
    means in the context of free format I don't know.
    '''
    def __init__(self, is_free, is_strict):
        '''
        Constructs a FortranFormat object from the describing parameters.

        Arguments:
            is_free   - (Boolean) True for free format, False for fixed.
            is_strict - (Boolean) Some amount of strictness.
        '''
        self._is_free = is_free
        self._is_strict = is_strict

    def __eq__(self, other):
        if isinstance(other, FortranFormat):
            return self._is_free == other.is_free \
                   and self._is_strict == other.is_strict
        raise NotImplementedError

    def __repr__(self):
        if self._is_strict:
            string = 'Strict'
        else:
            string = 'Non-strict'

        if self._is_free:
            string += ' free'
        else:
            string += ' fixed'

        return string + ' format'

    @property
    def is_free(self):
        '''
        Returns true if the "free format" flag is set.
        '''
        return self._is_free

    @property
    def is_strict(self):
        '''
        Returns true if the "strict" flag is set.
        '''
        return self._is_strict


##############################################################################

_HAS_F_EXTENSION = re.compile(r'.*[.](for|ftn|f77|f)\Z', re.I).match

_HAS_F_HEADER = re.compile(r'-[*]-\s*(fortran|f77)\s*-[*]-', re.I).search
_HAS_F90_HEADER = re.compile(r'-[*]-\s*f90\s*-[*]-', re.I).search
_HAS_F03_HEADER = re.compile(r'-[*]-\s*f03\s*-[*]-', re.I).search
_HAS_F08_HEADER = re.compile(r'-[*]-\s*f08\s*-[*]-', re.I).search
_HAS_FREE_HEADER = re.compile(r'-[*]-\s*(f90|f95|f03|f08)\s*-[*]-',
                              re.I).search
_HAS_FIX_HEADER = re.compile(r'-[*]-\s*fix\s*-[*]-', re.I).search
_HAS_PYF_HEADER = re.compile(r'-[*]-\s*pyf\s*-[*]-', re.I).search

_FREE_FORMAT_START = re.compile(r'[^c*!]\s*[^\s\d\t]', re.I).match


def get_source_info_str(source):
    '''
    Determines the format of Fortran source held in a string.

    Returns a FortranFormat object.
    '''
    lines = source.splitlines()
    if not lines:
        return FortranFormat(False, False)

    firstline = lines[0].lstrip()
    if _HAS_F_HEADER(firstline):
        return FortranFormat(False, True)
    if _HAS_FIX_HEADER(firstline):
        return FortranFormat(False, False)
    if _HAS_FREE_HEADER(firstline):
        return FortranFormat(True, False)
    if _HAS_PYF_HEADER(firstline):
        return FortranFormat(True, True)

    line_tally = 10000  # Check up to this number of non-comment lines
    is_free = False
    while line_tally > 0 and lines:
        line = lines.pop(0).rstrip()
        if line and line[0] != '!':
            line_tally -= 1
            if line[0] != '\t' and _FREE_FORMAT_START(line[:5]) \
               or line[-1:] == '&':
                is_free = True
                break

    return FortranFormat(is_free, False)


##############################################################################

def get_source_info(file_candidate):
    '''
    Determines the format of Fortran source held in a file.

    Returns a FortranFormat object.
    '''
    if hasattr(file_candidate, 'name'):
        filename = file_candidate.name
        # The behaviour of file.name when associated with a file without a
        # file name has changed between Python 2 and 3.
        #
        # Under Python 3 file.name holds an integer file handle.
        if isinstance(filename, int):
            filename = None

        # Under Python 2 file.name holds a string of the form "<..>".
        elif filename.startswith('<') and filename.endswith('>'):
            filename = None
    else:  # Candidate is a filename
        filename = file_candidate

    if filename:
        _, ext = os.path.splitext(filename)
        if ext == '.pyf':
            return FortranFormat(True, True)

    if hasattr(file_candidate, 'read'):
        pointer = file_candidate.tell()
        file_candidate.seek(0)
        source_info = get_source_info_str(file_candidate.read())
        file_candidate.seek(pointer)
        return source_info
    else:
        with open(file_candidate, 'r') as file_object:
            return get_source_info_str(file_object.read())

##############################################################################
