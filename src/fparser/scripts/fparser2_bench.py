#!/usr/bin/env python
# Copyright (c) 2022 Science and Technology Facilities Council
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

'''
Generates a large Fortran program in memory and then measures how long
it takes fparser2 to parse it. This is based on the benchmark suggested
by Ondřej Čertík via Ioannis Nikiteas.

'''
from fparser.common.sourceinfo import FortranFormat
from fparser.common.readfortran import FortranStringReader
from fparser.two.parser import ParserFactory
from time import perf_counter


def gen_sub(no: int):
    '''
    Constructs a Fortran subroutine named g<no>.

    :param no: the number of the subroutine (used to name it).

    :returns: Fortran subroutine.
    :rtype: str

    '''
    sub = """subroutine g{0}(x)
    integer, intent(inout) :: x
    integer :: i
    x = 0
    do i = {0}, {1}
        x = x+i
    end do
end subroutine

""".format(
        no, no + 9
    )
    return sub


def create_bench():
    '''
    :returns: benchmark Fortran code.
    :rtype: str
    '''
    # Number of subroutines to create.
    num_subroutines = 10000

    code = ["program bench3",
            "implicit none",
            "integer :: c",
            "c = 0"]
    for i in range(1, num_subroutines+1):
        code.append(f"call g{i}(c)")

    code.append("print *, c")
    code.append("contains")

    for i in range(1, num_subroutines+1):
        code.append(gen_sub(i))
    code.append("end program")

    return "\n".join(code)


if __name__ == "__main__":

    print("Constructing benchmark code...")
    code = create_bench()
    reader = FortranStringReader(code)
    # Ensure the reader uses free format.
    reader.set_format(FortranFormat(True, True))
    
    fparser = ParserFactory().create()

    print("Parsing benchmark code...")
    tstart = perf_counter()
    program = fparser(reader)
    tstop = perf_counter()

    print(f"Time taken for parse = {tstop - tstart:.2f}s")
