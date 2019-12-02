# This Python file uses the following encoding: utf-8
from fparser.common.readfortran import FortranFileReader, FortranStringReader
from fparser.two.Fortran2003 import Program, Case_Construct
import six

def test_tofortran_non_ascii(f2003_create):
    ''' Check that the tofortran() method works when the character string
    contains non-ascii characters. '''
    code = (u"SELECT CASE(iflag)\n"
            u"CASE(  30  ) ! This is a comment\n"
            u"  IF(lwp) WRITE(*,*) ' for e1=1\xb0'\n"
            u"END SELECT\n")
    reader = FortranStringReader(code, ignore_comments=False)
    obj = Case_Construct(reader)
    out_str = str(obj)
    assert "for e1=1" in out_str


def test_blockbase_tofortran_non_ascii(f2003_create):
    ''' Check that the tofortran() method works when the character string
    contains non-ascii characters. and there's a comment outside the select
    block. '''
    code = (u"program my_test\n"
            u"! A comment outside the select block\n"
            u"SELECT CASE(iflag)\n"
            u"CASE(  30  )\n"
            u"  IF(lwp) WRITE(*,*) ' for e1=1\xb0'\n"
            u"END SELECT\n"
            u"end program\n")
    reader = FortranStringReader(code, ignore_comments=False)
    obj = Program(reader)
    out_str = str(obj)
    assert "for e1=1" in out_str
