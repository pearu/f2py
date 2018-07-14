''' a simple example '''
from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader
myfile = '''
program hello
integer a
end program hello
submodule test
end submodule test
subroutine test2
end subroutine test2
'''
reader = FortranStringReader(myfile)
myparser = ParserFactory().create(std="f2008")
program = myparser(reader)
print program

