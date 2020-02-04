#!/usr/bin/env python

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

'''Test C99 Preprocessor directives: This file tests the preprocessor
directives defined in the C99 standard. Whilst this is not part of any
Fortran standard, most compilers support preprocessing of Fortran files
and support the majority of directives from the C-Standard. Since this
is actively used in user codes there are cases where users might like to
keep the directives in the Fortran parse tree and output it again.

'''
# Author: Balthasar Reuter <balthasar.reuter@ecmwf.int>
# Based on previous work by Martin Schlipf (https://github.com/martin-schlipf)
# First version created: Jan 2020

import pytest
from fparser.two.C99Preprocessor import (Cpp_If_Stmt, Cpp_Elif_Stmt,
                                         Cpp_Else_Stmt, Cpp_Endif_Stmt,
                                         Cpp_Include_Stmt, Cpp_Macro_Stmt,
                                         Cpp_Macro_Identifier, Cpp_Undef_Stmt,
                                         Cpp_Line_Stmt, Cpp_Error_Stmt,
                                         Cpp_Warning_Stmt, Cpp_Null_Stmt)
from fparser.two.parser import ParserFactory
from fparser.two.utils import NoMatchError
from fparser.api import get_reader


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
    for line in [None, '', ' ', '#esle', '#elseA', '#Aelse']:
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
    for line in [None, '', ' ', '#ednif', '#endifA', '#Aendif']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Endif_Stmt(line)
        assert "Cpp_Endif_Stmt: '{0}'".format(line) in str(excinfo.value)


def test_parse_define_outside_subroutine(f2003_create):
    f2003_parser = ParserFactory().create(std='f2003')

    code = '#define MACRO\nSUBROUTINE FOO\ncall sub()\nEND SUBROUTINE FOO\n'
    ref = '#define MACRO\nSUBROUTINE FOO\n  CALL sub\nEND SUBROUTINE FOO'
    reader = get_reader(ref)
    result = f2003_parser(reader)
    assert str(result) == ref

def test_parse_define_end_subroutine(f2003_create):
    f2003_parser = ParserFactory().create(std='f2003')
    ref = '''
SUBROUTINE FOO(BAR)
  INTEGER, INTENT(INOUT) :: BAR
  CALL FOOBAR(BAR)
  #define MACRO_NAME
END SUBROUTINE FOO
'''.strip()
    code = '''
SUBROUTINE FOO(BAR)
  INTEGER, INTENT(INOUT) :: BAR
  CALL FOOBAR(BAR)
  #define MACRO_NAME
END SUBROUTINE FOO
'''
    reader = get_reader(code)
    result = f2003_parser(reader)
    assert str(result) == ref

def test_parse_define_body_subroutine(f2003_create):
    f2003_parser = ParserFactory().create(std='f2003')
    ref = '''
SUBROUTINE FOO(BAZ)
  INTEGER, INTENT(INOUT) :: BAZ
  #define MACRO_NAME
  CALL FOOBAZ(BAR)
END SUBROUTINE FOO
'''.strip()
    code = '''
SUBROUTINE FOO(BAZ)
  INTEGER, INTENT(INOUT) :: BAZ
  #define MACRO_NAME
  CALL FOOBAZ(BAR)
END SUBROUTINE FOO
'''
    reader = get_reader(code)
    result = f2003_parser(reader)
    assert str(result) == ref

def test_parse_define_in_specification(f2003_create):
    f2003_parser = ParserFactory().create(std='f2003')
    ref = '''
SUBROUTINE FOO(BAZ2)
  #define MACRO_NAME
  INTEGER, INTENT(INOUT) :: BAZ2
  CALL FOOBAZ2(BAR)
END SUBROUTINE FOO
'''.strip()
    code = '''
SUBROUTINE FOO(BAZ2)
  #define MACRO_NAME
  INTEGER, INTENT(INOUT) :: BAZ2
  CALL FOOBAZ2(BAR)
END SUBROUTINE FOO
'''
    reader = get_reader(code)
    result = f2003_parser(reader)
    assert str(result) == ref

def test_parse_ifdef_in_body(f2003_create):
    f2003_parser = ParserFactory().create(std='f2003')
    ref = '''
SUBROUTINE FOO(BAR2)
  INTEGER, INTENT(INOUT) :: BAR2
  #ifdef MACRO_NAME
  CALL FOOBAR2(BAR)
  #else
  CALL ABORT("error message")
  #endif
  BAR2 = BAR2 + 1
END SUBROUTINE FOO
'''.strip()
    code = '''
SUBROUTINE FOO(BAR2)
  INTEGER, INTENT(INOUT) :: BAR2
#ifdef MACRO_NAME
  CALL FOOBAR2(BAR)
#else
  CALL ABORT("error message")
#endif
  BAR2 = BAR2 + 1
END SUBROUTINE FOO
'''
    reader = get_reader(code)
    result = f2003_parser(reader)
    assert str(result) == ref

def test_parse_ifdef_in_subroutine(f2003_create):
    f2003_parser = ParserFactory().create(std='f2003')
    ref = '''
SUBROUTINE FOO(BAR3)
  #ifdef MACRO_NAME
  INTEGER, INTENT(INOUT) :: BAR3
  CALL FOOBAR3(BAR)
  #endif
END SUBROUTINE FOO
'''.strip()
    code = '''
SUBROUTINE FOO(BAR3)
#ifdef MACRO_NAME
  INTEGER, INTENT(INOUT) :: BAR3
  CALL FOOBAR3(BAR)
#endif
END SUBROUTINE FOO
'''
    reader = get_reader(code)
    result = f2003_parser(reader)
    assert str(result) == ref

def test_parse_ifdef_elif_in_subroutine(f2003_create):
    f2003_parser = ParserFactory().create(std='f2003')
    ref = '''
#define MACRO
SUBROUTINE BAR
  #if defined(MACRO)
  CALL sub1
  #elif FOO
  CALL sub2
  #else
  CALL sub3
  #endif
END SUBROUTINE BAR'''.strip()
    code = '''
#define MACRO
SUBROUTINE BAR
#if defined(MACRO)
  call sub1()
#elif FOO
  call sub2()
#else
  call sub3()
#endif
END SUBROUTINE BAR
'''
    reader = get_reader(code)
    result = f2003_parser(reader)
    assert str(result) == ref


def test_include_stmt(f2003_create):
    '''Test that #include is recognized'''
    ref = '#include "filename.inc"'
    code = ['#include "filename.inc"', '   #   include  "filename.inc"  ']
    for line in code:
        result = Cpp_Include_Stmt(line)
        assert str(result) == ref


def test_incorrect_include_stmt(f2003_create):
    '''Test that incorrectly formed #include statements raise exception'''
    code = [None, '', '  ', '#includ', '#includ "x"', '#include',
            '#include ""', "#include 'x'", '#include "x', '#include x"',
            '#include x', '#include x"x"', '#include "x"x', 'x #include "x"',
            '#includex "x"', "#include 'abc'"]
    for line in code:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Include_Stmt(line)
        assert "Cpp_Include_Stmt: '{0}'".format(line) in str(excinfo.value)


def test_macro_stmt(f2003_create):
    '''Test that #define is recognized'''
    # definition with value
    ref = '#define MACRO value'
    for line in ['#define MACRO value',
                 '  #  define   MACRO   value  ']:
        result = Cpp_Macro_Stmt(line)
        assert str(result) == ref
    # definition without value
    ref = '#define _MACRO'
    for line in ['#define _MACRO',
                 '   #  define  _MACRO  ']:
        result = Cpp_Macro_Stmt(line)
        assert str(result) == ref
    # more definitions with parameters and similar
    code = ['#define MACRO(x) call func(x)',
            '#define MACRO (x, y)',
            '#define MACRO(x, y) (x) + (y)',
            '#define MACRO(a, b, c, d) (a) * (b) + (c) * (d)',
            '#define eprintf(...) fprintf (stderr, __VA_ARGS__)',
            '#define report(tst, ...) ((tst)?puts(#tst):printf(__VA_ARGS__))',
            '#define hash_hash # ## #',
            '#define TABSIZE 100',
            '#define r(x,y) x ## y',
            '#define MACRO(a, b, c) (a) * (b + c)',
            '#define MACRO( a,b ,   c) (a )*    (   b   + c  )']
    for ref in code:
        result = Cpp_Macro_Stmt(ref)
        assert str(result) == ref


def test_incorrect_macro_stmt(f2003_create):
    '''Test that incorrectly formed #define statements raise exception'''
    for line in [None, '', ' ', '#def', '#defnie', '#definex',
                 '#define fail(...,test) test', '#define']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Macro_Stmt(line)
        assert "Cpp_Macro_Stmt: '{0}'".format(line) in str(excinfo.value)


def test_macro_identifier(f2003_create):
    '''Test that all allowed names can be parsed'''
    for name in ['MACRO', 'MACRO1', 'MACRO_', 'MACRO_NAME', 'macro',
                 '_', '_12']:
        result = Cpp_Macro_Identifier(name)
        assert str(result) == name


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


def test_line_statement(f2003_create):
    '''Test that #line is recognized'''
    ref = '#line CONFIG'
    for line in [
        '#line CONFIG',
        '  #  line   CONFIG  ',
    ]:
        result = Cpp_Line_Stmt(line)
        assert str(result) == ref


def test_incorrect_line_stmt(f2003_create):
    '''Test that incorrectly formed #line statements raise exception'''
    for line in [None, '', ' ', '#line', '#linex', '#lien']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Line_Stmt(line)
        assert "Cpp_Line_Stmt: '{0}'".format(line) in str(excinfo.value)


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
    '''Test that #warning is recognized (not actually part of C99)'''
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


def test_null_stmt(f2003_create):
    '''Test that null directives are recognized'''
    ref = '#'
    for line in ['#', '   #  ', '#   ', '   #']:
        result = Cpp_Null_Stmt(line)
        assert str(result) == ref


def test_incorrect_null_stmt(f2003_create):
    '''Test that anything that is not a single hash sign is not recognized'''
    for line in [None, '', ' ', '#a', '##', '###', '  # #', '# a']:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Cpp_Null_Stmt(line)
        assert "Cpp_Null_Stmt: '{0}'".format(line) in str(excinfo.value)
