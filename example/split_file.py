#!/usr/bin/env python

# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# ------------------------------------------------------------------------------
# Author: Joerg Henrichs, Bureau of Meteorology

"""This file contains an fparser script that splits a source file that contains
several modules and/or subroutines into individual files. Note that existing
files will be overwritten, so it is recommended to run this script in a clean
directory.

Usage:  split_file.py  file.f90
"""


import os
import subprocess
import sys

from fparser.common.readfortran import FortranFileReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory


# -----------------------------------------------------------------------------
def usage():
    """This function prints the usage information and exits. It is called if
    incorrect input parameters are supplied.
    """
    print(f"{sys.argv[0]} file1")
    sys.exit(-1)


# -----------------------------------------------------------------------------
if __name__ == "__main__":
    if len(sys.argv) != 2:
        usage()

    # Extension to use for the files that are being created
    EXT = ".F90"

    try:
        reader = FortranFileReader(sys.argv[1])
    except IOError:
        print(f"Could not open file '{sys.argv[1]}'.", file=sys.stderr)
        sys.exit(-1)

    Parser = ParserFactory().create(std="f2008")
    parse_tree = Parser(reader)

    all_filenames = []
    all_objs = []
    # pylint considers this a constant??
    # pylint: disable=invalid-name
    main_name = ""
    # pylint: enable=invalid-name
    for unit in parse_tree.children:
        # For Module, Subroutine_Subprogram, Function_Subprogram, Main_Program
        # we always get the name using this:
        unit_name = unit.children[0].items[1].string

        # Save the name of the main program if we have one, which will be
        # used in the Makefile created later
        if isinstance(unit, Fortran2003.Main_Program):
            main_name = unit_name

        filename = f"{unit_name}{EXT}"
        if os.path.exists(filename):
            print(f"The file '{filename}' already exists - aborting.")
            sys.exit(-1)
        with open(filename, mode='w', encoding='utf-8') as f_out:
            f_out.write(str(unit))
        all_filenames.append(filename)
        all_objs.append(f"{unit_name}.o")

    file_path = os.path.dirname(os.path.realpath(__file__))
    completed = subprocess.run(
        [f"{file_path}/create_dependencies.py"]+all_filenames,
        capture_output=True, check=True)

    # Query some environment flags which will define the
    # defaults in the Makefile
    f90 = os.getenv("F90", "gfortran")
    f90flags = os.getenv("F90FLAGS", "-g -O0")
    ldflags = os.getenv("LDFLAGS", "")

    # Now create a makefile
    with open("Makefile", mode='w', encoding='utf-8') as f_out:
        f_out.write(f"F90?={f90}\n")
        f_out.write(f"F90FLAGS?={f90flags}\n")
        f_out.write(f"LDFLAGS?={ldflags}\n")
        f_out.write("\n")
        f_out.write(f"OBJS={' '.join(all_objs)}\n")
        f_out.write("\n")
        if main_name:
            f_out.write(f"default: {main_name}\n")
            f_out.write("\n")
            f_out.write(f"{main_name}: $(OBJS)\n")
            f_out.write(f"\t$(F90) $(F90FLAGS) $(OBJS) -o {main_name} "
                        f"$(LDFLAGS)n")
            f_out.write("\n")
        else:
            f_out.write("default: $(OBJS)\n")
        f_out.write("\n# Dependencies\n# ============\n")
        f_out.write(completed.stdout.decode("utf-8"))
        f_out.write("\n# Compilation rules\n# =================\n")
        f_out.write(f"%.o: %{EXT}\n")
        f_out.write("\t$(F90) $(F90FLAGS) -c $<\n")
        f_out.write("\n")
        f_out.write("\n# Cleanup\n# =======\n")
        f_out.write("clean:\n")
        if main_name:
            f_out.write(f"\trm -f {main_name} $(OBJS)")
        else:
            f_out.write("\trm -f $(OBJS)")
