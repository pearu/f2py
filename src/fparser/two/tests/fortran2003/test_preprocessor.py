import pytest
from fparser.two.Fortran2003 import (Cpp_Include_Stmt, Cpp_Define_Stmt, Cpp_If_Stmt,
    Cpp_Elif_Stmt, Cpp_Endif_Stmt)
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

def test_if_stmt(f2003_create):
    '''Test that various forms of #if, #ifdef, #ifndef are recognized'''
    ref = '#if CONSTANT'
    for line in [
        '#if CONSTANT',
        '  #  if    CONSTANT  ',
    ]:
        result = Cpp_If_Stmt(line)
        assert str(result) == ref
    ref = '#ifdef MACRO'
    for line in [
        '#ifdef MACRO',
        '  #  ifdef  MACRO  '
    ]:
        result = Cpp_If_Stmt(line)
        assert str(result) == ref
    ref = '#ifndef MACRO'
    for line in [
        '#ifndef MACRO',
        '  #  ifndef  MACRO  '
    ]:
        result = Cpp_If_Stmt(line)
        assert str(result) == ref

def test_incorrect_if_stmt(f2003_create):
    '''Test that incorrectly formed #if statements raise exception'''
    for line in [None, '', ' ', '#ifdfe', '#if', '#ifdef', '#ifdef two macros']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_If_Stmt(line)
        assert "Cpp_If_Stmt: '{0}'".format(line) in str(excinfo.value)

def test_elif_stmt(f2003_create):
    '''Test that #elif is correctly recognized'''
    ref = '#elif CONDITION'
    for line in [
        '#elif CONDITION',
        '  #  elif   CONDITION  ',
    ]:
        result = Cpp_Elif_Stmt(line)
        assert str(result) == ref

def test_incorrect_elif_stmt(f2003_create):
    '''Test that incorrectly formed #elif statements raise exception'''
    for line in [None, '', ' ', '#elfi', '#elif']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Elif_Stmt(line)
        assert "Cpp_Elif_Stmt: '{0}'".format(line) in str(excinfo.value)

def test_endif_stmt(f2003_create):
    '''Test that #endif is correctly recognized'''
    ref = '#endif'
    for line in [
        '#endif',
        '  #  endif  ',
    ]:
        result = Cpp_Endif_Stmt(line)
        assert str(result) == ref

def test_incorrect_endif_stmt(f2003_create):
    '''Test that incorrectly formed #endif statements raise exception'''
    for line in [None, '', ' ', '#ednif', '#endif text']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Endif_Stmt(line)
        assert "Cpp_Endif_Stmt: '{0}'".format(line) in str(excinfo.value)
