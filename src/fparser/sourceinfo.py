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

"""
Provides get_source_info(<filename>) function to determine the format
(free|fixed|strict|pyf) of a Fortran file.

-----
Permission to use, modify, and distribute this software is given under the
terms of the NumPy License. See http://scipy.org.

NO WARRANTY IS EXPRESSED OR IMPLIED.  USE AT YOUR OWN RISK.
Author: Pearu Peterson <pearu@cens.ioc.ee>
Created: May 2006
-----
"""

__all__ = ['get_source_info', 'get_source_info_str']

import re
import os
import sys

_has_f_extension = re.compile(r'.*[.](for|ftn|f77|f)\Z',re.I).match
_has_f_header = re.compile(r'-[*]-\s*(fortran|f77)\s*-[*]-',re.I).search
_has_f90_header = re.compile(r'-[*]-\s*f90\s*-[*]-',re.I).search
_has_f03_header = re.compile(r'-[*]-\s*f03\s*-[*]-',re.I).search
_has_f08_header = re.compile(r'-[*]-\s*f08\s*-[*]-',re.I).search
_has_free_header = re.compile(r'-[*]-\s*(f90|f95|f03|f08)\s*-[*]-',re.I).search
_has_fix_header = re.compile(r'-[*]-\s*fix\s*-[*]-',re.I).search
_has_pyf_header = re.compile(r'-[*]-\s*pyf\s*-[*]-',re.I).search
_free_format_start = re.compile(r'[^c*!]\s*[^\s\d\t]',re.I).match

def get_source_info(filename):
    """
    Determine if fortran file is
      - in fix format and contains Fortran 77 code    -> return False, True
      - in fix format and contains Fortran 90,95,2003,2008 code    -> return False, False
      - in free format and contains Fortran 90,95,2003,2008 code   -> return True, False
      - in free format and contains signatures (.pyf) -> return True, True
    """
    base,ext = os.path.splitext(filename)
    if ext=='.pyf':
        return True, True
    isfree = False
    isstrict = False
    f = open(filename,'r')
    firstline = f.readline()
    f.close()
    if _has_f_header(firstline): return False, True
    if _has_fix_header(firstline): return False, False
    if _has_free_header(firstline): return True, False
    if _has_pyf_header(firstline): return True, True
    if _has_f_extension(filename) and \
       not (_has_free_header(firstline) or _has_fix_header(firstline)):
        isstrict = True
    elif is_free_format(filename) and not _has_fix_header(firstline):
        isfree = True
    return isfree,isstrict

def get_source_info_str(code):
    """
    Determine the format of Fortran code.
    See get_source_info() for the meaning of return values.
    """
    firstline = code.lstrip().split('\n',1)[0]
    if _has_f_header(firstline): return False, True
    if _has_fix_header(firstline): return False, False
    if _has_free_header(firstline): return True, False
    if _has_pyf_header(firstline): return True, True
    n = 10000 # the number of non-comment lines to scan for hints
    lines = code.splitlines()
    isfree = False
    while n>0 and lines:
        line = lines.pop(0).rstrip()
        if line and line[0]!='!':
            n -= 1
            if line[0]!='\t' and _free_format_start(line[:5]) or line[-1:]=='&':
                isfree = True
                break
        
    return isfree, False # assume free f90 or fix f90.

def is_free_format(file):
    """Check if file is in free format Fortran."""
    # f90-2008 allows both fixed and free format, assuming fixed
    # unless signs of free format are detected.
    isfree = False
    f = open(file,'r')
    line = f.readline()
    n = 10000 # the number of non-comment lines to scan for hints
    if _has_f_header(line):
        n = 0
    elif _has_free_header(line):
        n = 0
        isfree = True
    while n>0 and line:
        line = line.rstrip()
        if line and line[0]!='!':
            n -= 1
            if line[0]!='\t' and _free_format_start(line[:5]) or line[-1:]=='&':
                isfree = True
                break
        line = f.readline()
    f.close()
    return isfree

def simple_main():
    for filename in sys.argv[1:]:
        isfree, isstrict = get_source_info(filename)
        print '%s: isfree=%s, isstrict=%s'  % (filename, isfree, isstrict)

if __name__ == '__main__':
    simple_main()
