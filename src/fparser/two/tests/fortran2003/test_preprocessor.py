import pytest
from fparser.two.Fortran2003 import (Cpp_Include_Stmt, Cpp_Define_Stmt, Cpp_Undef_Stmt,
    Cpp_If_Stmt, Cpp_Elif_Stmt, Cpp_Else_Stmt, Cpp_Endif_Stmt, Cpp_If_Construct,
    Cpp_Error_Stmt, Cpp_Warning_Stmt, Cpp_Line_Stmt, Cpp_Macro)
from fparser.two.utils import NoMatchError
from fparser.api import get_reader

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
    ref = '#define _MACRO'
    for line in [
        '#define _MACRO',
        '   #  define  _MACRO  ',
    ]:
        result = Cpp_Define_Stmt(line)
        assert str(result) == ref

def test_incorrect_define_stmt(f2003_create):
    '''Test that incorrectly formed #define statements raises exception'''
    for line in [None, '', ' ', '#def', '#defnie', '#definex']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Define_Stmt(line)
        assert "Cpp_Define_Stmt: '{0}'".format(line) in str(excinfo.value)

def test_undef_stmt(f2003_create):
    '''Test that #undef is recognized'''
    ref = '#undef _MACRO'
    for line in [
        '#undef _MACRO',
        '   #  undef  _MACRO  ',
    ]:
        result = Cpp_Undef_Stmt(line)
        assert str(result) == ref

def test_incorrect_undef_stmt(f2003_create):
    '''Test that incorrectly formed #undef statements raise exception'''
    for line in [None, '', ' ', '#undef', '#unfed', '#undefx']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Undef_Stmt(line)
        assert "Cpp_Undef_Stmt: '{0}'".format(line) in str(excinfo.value)

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
    ref = '#ifndef _MACRO'
    for line in [
        '#ifndef _MACRO',
        '  #  ifndef  _MACRO  '
    ]:
        result = Cpp_If_Stmt(line)
        assert str(result) == ref

def test_incorrect_if_stmt(f2003_create):
    '''Test that incorrectly formed #if statements raise exception'''
    for line in [None, '', ' ', '#ifdfe', '#if', '#ifdef', '#ifdef two macros']:
        with pytest.raises(NoMatchError) as excinfo:
            print(line)
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

def test_else_stmt(f2003_create):
    '''Test that #else is correctly recognized'''
    ref = '#else'
    for line in [
        '#else',
        '  # else  ',
    ]:
        result = Cpp_Else_Stmt(line)
        assert str(result) == ref

def test_incorrect_else_stmt(f2003_create):
    '''Test that incorrectly formed #else statements raise exception'''
    for line in [None, '', ' ', '#esle', '#else text']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Else_Stmt(line)
        assert "Cpp_Else_Stmt: '{0}'".format(line) in str(excinfo.value)

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

def test_if_construct(f2003_create):
    ref = '''
#if defined(MACRO)
  CALL sub1
#elif FOO
  CALL sub2
#else
  CALL sub3
#endif
'''.strip()
    reader = get_reader('''
#if defined(MACRO)
    call sub1()
#elif FOO
    call sub2()
#else
    call sub3()
#endif
    ''')
    result = Cpp_If_Construct(reader)
    assert str(result) == ref

def test_error_statement(f2003_create):
    '''Test that #error is recognized'''
    # error with message
    ref = '#error MSG'
    for line in [
        '#error MSG',
        '  #  error  MSG  ',
    ]:
        result = Cpp_Error_Stmt(line)
        assert str(result) == ref
    # error without message
    ref = '#error'
    for line in [
        '#error',
        '   #  error  ',
    ]:
        result = Cpp_Error_Stmt(line)
        assert str(result) == ref

def test_incorrect_error_stmt(f2003_create):
    '''Test that incorrectly formed #error statements raise exception'''
    for line in [None, '', ' ', '#erorr', '#errorx']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Error_Stmt(line)
        assert "Cpp_Error_Stmt: '{0}'".format(line) in str(excinfo.value)

def test_warning_statement(f2003_create):
    '''Test that #warning is recognized'''
    # warning with message
    ref = '#warning MSG'
    for line in [
        '#warning MSG',
        '  #  warning  MSG  ',
    ]:
        result = Cpp_Warning_Stmt(line)
        assert str(result) == ref
    # warning without message
    ref = '#warning'
    for line in [
        '#warning',
        '   #  warning  ',
    ]:
        result = Cpp_Warning_Stmt(line)
        assert str(result) == ref

def test_incorrect_warning_stmt(f2003_create):
    '''Test that incorrectly formed #warning statements raise exception'''
    for line in [None, '', ' ', '#wrning', '#warningx']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Warning_Stmt(line)
        assert "Cpp_Warning_Stmt: '{0}'".format(line) in str(excinfo.value)

def test_line_statement(f2003_create):
    '''Test that #line is recognized'''
    ref = '#line CONFIG'
    for line in [
        '#line CONFIG',
        '  #  line   CONFIG  ',
    ]:
        result = Cpp_Line_Stmt(line)
        assert str(result) == ref

def test_incorrect_warning_stmt(f2003_create):
    '''Test that incorrectly formed #line statements raise exception'''
    for line in [None, '', ' ', '#line', '#linex', '#lien']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Line_Stmt(line)
        assert "Cpp_Line_Stmt: '{0}'".format(line) in str(excinfo.value)

def test_macro(f2003_create):
    '''Test that all allowed names can be parsed'''
    for name in ['MACRO', 'MACRO1', 'MACRO_', 'macro', '_', '_12']:
        result = Cpp_Macro(name)
        assert str(result) == name
