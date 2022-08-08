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

from fparser.common.readfortran import FortranStringReader
from fparser.two.parser import ParserFactory
from fparser.two.utils import walk
from fparser.two.Fortran2003 import Name, Block_Nonlabel_Do_Construct

def test_reproduce_issue367():
    ARG_FORTRAN_STANDARD = "f2008"

    source_str = """\
program test
    integer :: iterator, size
    integer, dimension(0:10) :: A, B, C
    size = 10
    do iterator = 0,size
      A(iterator) = B(iterator) * C(iterator)
    enddo
end program test
"""
    expected = {"A", "B", "C", "iterator", "size"}

    reader = FortranStringReader(source_str, ignore_comments=False)
    f_parser = ParserFactory().create(std=ARG_FORTRAN_STANDARD)
    parse_tree = f_parser(reader)
    doloops = walk(parse_tree, Block_Nonlabel_Do_Construct)
    assert len(doloops) == 1
    doloop = doloops[0]
    allNames = walk(doloop, Name, 0)
    assert len(allNames) == 8
    identifierSet = set(map(lambda x: x.tostr(), allNames))
    assert identifierSet == expected
