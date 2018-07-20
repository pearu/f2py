'''A simple fparser2 Fortran2008 example demonstrating support for
submodules'''
from __future__ import absolute_import
from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader
MYFILE = '''
program hello
integer a
end program hello
submodule (hello2) world
end submodule world
subroutine world2
end subroutine world2
'''
READER = FortranStringReader(MYFILE)
F2008_PARSER = ParserFactory().create(std="f2008")
PROGRAM = F2008_PARSER(READER)
print(PROGRAM)
