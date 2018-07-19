''' A simple fparser2 Fortran2008 example demonstrating support for submodules '''
from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader
myfile = '''
program hello
integer a
end program hello
submodule (hello2) world
end submodule world
subroutine world2
end subroutine world2
'''
reader = FortranStringReader(myfile)
f2008_parser = ParserFactory().create(std="f2008")
program = f2008_parser(reader)
print program

