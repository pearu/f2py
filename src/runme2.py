''' a simple example '''
from fparser.two import parser
#from fparser.two import Fortran2003
from fparser.common.readfortran import FortranFileReader
myfile = "test.f90"
reader = FortranFileReader(myfile)
myparser = parser.ParserFactory().create(std="f2008")
#program = Fortran2003.Program(reader)
program = myparser(reader)
print program
#program

