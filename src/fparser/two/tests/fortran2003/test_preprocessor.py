import pytest
from fparser.two.Fortran2003 import Cpp_Include_Stmt, Cpp_Define_Stmt
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
    '''Test that incorrectly formed #include statements raises exception'''
    for line in [None, '', '  ', '#includ', '#includ "x"', '#include', '#include ""',
            "#include 'x'", '#include "x', '#include x"', '#include x',
            '#include x"x"', '#include "x"x', 'x #include "x"', '#includex "x"']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Include_Stmt(line)
        assert "Cpp_Include_Stmt: '{0}'".format(line) in str(excinfo.value)

def test_define_stmt(f2003_create):
    '''Test that #define is recognized'''
    # definition with value
    ref = '#define MACRO value'
    for line in [
        '#define MACRO value',
        '  #  define   MACRO   value  ',
    ]:
        result = Cpp_Define_Stmt(line)
        assert str(result) == ref
    # definition without value
    ref = '#define MACRO'
    for line in [
        '#define MACRO',
        '   #  define  MACRO  ',
    ]:
        result = Cpp_Define_Stmt(line)
        assert str(result) == ref

def test_incorrect_define_stmt(f2003_create):
    '''Test that incorrectly formed #define statements raises exception'''
    for line in [None, '', ' ', '#def', '#defnie', '#definex']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Define_Stmt(line)
        assert "Cpp_Define_Stmt: '{0}'".format(line) in str(excinfo.value)
