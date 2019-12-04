import pytest
from fparser.two.Fortran2003 import Cpp_Include_Stmt
from fparser.two.utils import NoMatchError

def test_include_stmt(f2003_create):
    '''Test that #include is recognized'''
    ref = '#include "filename.inc"'
    for line in [
        '#include "filename.inc"',
        '   #   include  "filename.inc"  '
    ]:
        result = Cpp_Include_Stmt(line)
        assert str(result) == ref

def test_incorrect_include_stmt(f2003_create):
    '''Test that incorrectly formed #include statements return None'''
    for line in [None, '', '  ', '#includ', '#includ "x"', '#include', '#include ""',
            "#include 'x'", '#include "x', '#include x"', '#include x',
            '#include x"x"', '#include "x"x', 'x #include "x"']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Include_Stmt(line)
        assert "Cpp_Include_Stmt: '{0}'".format(line) in str(excinfo.value)
