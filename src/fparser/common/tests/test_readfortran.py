# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017-2019 Science and Technology Facilities Council
#
# All rights reserved.
#
# Modifications made as part of the fparser project are distributed
# under the following license:
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
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
##############################################################################
# Modified M. Hambley and P. Elson, Met Office
# Modified R. W. Ford and A. R. Porter, STFC Daresbury Lab
##############################################################################
'''
Test battery associated with fparser.common.readfortran package.
'''
from __future__ import print_function

import io
import os.path
import tempfile
import six
import re
import pytest

from fparser.common.readfortran import FortranFileReader, FortranStringReader
import fparser.common.sourceinfo
import fparser.common.tests.logging_utils


def test_empty_line_err():
    ''' Check that we raise the expected error if we try and create
    an empty Line '''
    from fparser.common.readfortran import Line, FortranReaderError
    with pytest.raises(FortranReaderError) as err:
        _ = Line("   ", 1, "", "a_name", None)
    assert "Got empty line: \'   \'" in str(err.value)


def test_111fortranreaderbase(log, monkeypatch):
    '''
    Tests the FortranReaderBase class.

    Currently only tests logging functionality.
    '''
    class FailFile(object):
        '''
        A "file-like" object which returns a line of Fortran source followed
        by raising a StopIteration exception.
        '''
        _stuff = ['x=1']

        def next(self):
            '''
            Used by Python 2.7.
            '''
            return self.__next__()

        def __next__(self):
            '''
            Used by Python 3.
            '''
            return self._stuff.pop()

    monkeypatch.setattr('fparser.common.readfortran.FortranReaderBase.id',
                        lambda x: 'foo', raising=False)
    mode = fparser.common.sourceinfo.FortranFormat(True, False)
    unit_under_test = fparser.common.readfortran.FortranReaderBase(FailFile(),
                                                                   mode,
                                                                   True)
    assert str(unit_under_test.next()) == "line #1'x=1'"
    with pytest.raises(StopIteration):
        unit_under_test.next()

    assert log.messages['info'] == []
    assert log.messages['warning'] == []
    assert log.messages['error'] == []
    result = log.messages['critical'][0].split('\n')[1]
    assert result == '    1:x=1 <== while processing line'
    assert log.messages['critical'][1] == 'STOPPED READING'
    expected = 'Traceback\n'
    assert log.messages['debug'][0][:len(expected)] == expected


def test_include_not_found():
    '''Tests that FortranReaderBase.next() provides the include line when
    the included file is not found.

    '''
    code = "include 'nonexistant.f90'"
    unit_under_test = fparser.common.readfortran.FortranStringReader(code)
    line = unit_under_test.next()
    assert str(line.line) == code


def test_base_next_good_include(log):
    '''
    Tests that FortranReaderBase.next() causes a message to be logged when a
    file is included.
    '''
    code = "include 'modfile.f95'\nx=2"
    include_directories = [os.path.dirname(__file__)]
    unit_under_test = fparser.common.readfortran \
        .FortranStringReader(code, include_dirs=include_directories,
                             ignore_comments=False)
    line = unit_under_test.next()
    assert str(line)[:19] == "Comment('! Modified"  # First line of inclusion
    assert log.messages['debug'] == []
    assert log.messages['error'] == []
    assert log.messages['warning'] == []
    assert log.messages['critical'] == []
    expected = "    1:include 'modfile.f95' " \
               + "<== including file '{path}/modfile.f95'"
    result = log.messages['info'][0].split('\n')[1]
    assert re.sub("u", "", result) == \
        re.sub("u", "", expected.format(path=include_directories[0]))


def test_fortranreaderbase_info(log):
    '''
    Tests that FortranReaderBase.info() causes a message to be logged.
    '''
    unit_under_test = fparser.common.readfortran.FortranStringReader('x=3')
    thing = unit_under_test.get_source_item()
    unit_under_test.info('Mighty Whirlitzer', thing)
    assert log.messages['debug'] == []
    assert log.messages['error'] == []
    assert log.messages['warning'] == []
    assert log.messages['critical'] == []
    expected = '    1:x=3 <== Mighty Whirlitzer'
    result = log.messages['info'][0].split('\n')[1]
    assert result == expected


def test_fortranreaderbase_error(log):
    '''
    Tests that FortranReaderBase.error() causes a message to be logged.
    '''
    unit_under_test = fparser.common.readfortran.FortranStringReader('x=2')
    thing = unit_under_test.get_source_item()
    with pytest.raises(SystemExit):
        unit_under_test.error('Thundering Chalmer', thing)
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['warning'] == []
    assert log.messages['critical'] == []
    expected = '    1:x=2 <== Thundering Chalmer'
    result = log.messages['error'][0].split('\n')[1]
    assert result == expected


def test_fortranreaderbase_warning(log):
    '''
    Tests that FortranReaderBase.warning() causes a message to be logged.
    '''
    unit_under_test = fparser.common.readfortran.FortranStringReader('x=1')
    thing = unit_under_test.get_source_item()
    unit_under_test.warning('Flatulent Hermit', thing)
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'] == []
    expected = '    1:x=1 <== Flatulent Hermit'
    result = log.messages['warning'][0].split('\n')[1]
    assert result == expected


def test_base_handle_multilines(log):
    '''
    Tests that FortranReaderBase.get_source_item() logs the correct messages
    when there are quote discrepancies.
    '''
    code = 'character(8) :: test = \'foo"""bar'
    log.reset()
    unit_under_test = fparser.common.readfortran.FortranStringReader(code)
    mode = fparser.common.sourceinfo.FortranFormat(True, True)
    unit_under_test.set_format(mode)  # Force strict free format
    unit_under_test.get_source_item()
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'] == []
    expected = 'multiline prefix contains odd number of "\'" characters'
    result = log.messages['warning'][0].split('<==')[1].lstrip()
    assert result == expected

    code = 'goo """boo\n doo""" soo \'foo'
    log.reset()
    unit_under_test = fparser.common.readfortran.FortranStringReader(code)
    mode = fparser.common.sourceinfo.FortranFormat(True, True)
    unit_under_test.set_format(mode)  # Force strict free format
    unit_under_test.get_source_item()
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'] == []
    expected = 'following character continuation: "\'", expected None.'
    result = log.messages['warning'][0].split('<==')[1].lstrip()
    assert result == expected


def test_base_fixed_nonlabel(log):
    '''
    Tests that FortranReaderBase.get_source_item() logs the correct messages
    when there is an unexpected character in the initial 6 columns.
    '''
    # Checks that a bad character in the first column causes an event to be
    # logged.
    code = 'w    integer :: i'
    log.reset()
    unit_under_test = fparser.common.readfortran.FortranStringReader(code)
    mode = fparser.common.sourceinfo.FortranFormat(False, True)
    unit_under_test.set_format(mode)  # Force fixed format
    unit_under_test.get_source_item()
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'] == []
    result = log.messages['warning'][0].split('<==')[1].lstrip()
    expected = "non-space/digit char 'w' found in column 1 of fixed " \
               + "Fortran code, interpreting line as comment line"
    assert result == expected

    # Checks a bad character in columns 2-6
    for i in range(1, 5):
        code = ' '*i + 'w' + ' '*(5-i) + 'integer :: i'
        log.reset()
        unit_under_test = fparser.common.readfortran.FortranStringReader(code)
        mode = fparser.common.sourceinfo.FortranFormat(False, True)
        unit_under_test.set_format(mode)  # Force strict fixed format
        unit_under_test.get_source_item()
        assert log.messages['debug'] == []
        assert log.messages['info'] == []
        assert log.messages['error'] == []
        assert log.messages['critical'] == []
        result = log.messages['warning'][0].split('<==')[1].lstrip()
        expected = "non-space/digit char 'w' found in column {col} " \
                   + "of fixed Fortran code"
        assert result == expected.format(col=i+1)

    # Checks for a bad character, not in the first column, with "sloppy" mode
    # engaged.
    code = ' w   integer :: i'
    log.reset()
    unit_under_test = fparser.common.readfortran.FortranStringReader(code)
    mode = fparser.common.sourceinfo.FortranFormat(False, False)
    unit_under_test.set_format(mode)  # Force sloppy fixed format
    unit_under_test.get_source_item()
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'] == []
    expected = "non-space/digit char 'w' found in column 2 " \
               + "of fixed Fortran code, switching to free format mode"
    result = log.messages['warning'][0].split('<==')[1].lstrip()
    assert result == expected


def test_base_fixed_continuation(log):
    '''
    Tests that FortranReaderBase.get_source_item() logs the correct messages
    when there are quote mismatches across a continuation in fixed format.
    '''
    code = '     character(4) :: cheese = "a & !\n     & b'
    log.reset()
    unit_under_test = fparser.common.readfortran.FortranStringReader(code)
    mode = fparser.common.sourceinfo.FortranFormat(False, False)
    unit_under_test.set_format(mode)  # Force sloppy fixed format
    unit_under_test.get_source_item()
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'] == []
    expected = 'following character continuation: \'"\', expected None.'
    result = log.messages['warning'][0].split('<==')[1].lstrip()
    assert result == expected

    code = '     x=1 &\n     +1 &\n     -2'
    log.reset()
    unit_under_test = fparser.common.readfortran.FortranStringReader(code)
    mode = fparser.common.sourceinfo.FortranFormat(False, False)
    unit_under_test.set_format(mode)  # Force sloppy fixed format
    unit_under_test.get_source_item()
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['error'] == []
    assert log.messages['critical'] == []
    expected = 'free format line continuation character `&\' detected ' \
               + 'in fix format code\n    2:     +1 &\n    3:     -2'
    result = log.messages['warning'][0].split('<==')[1].lstrip()
    assert result == expected


def test_base_free_continuation(log):
    '''
    Tests that FortranReaderBase.get_source_item() logs the correct messages
    when there are quote mismatches across a continuation in free format.
    '''
    code = 'character(4) :: "boo & que'
    log.reset()
    unit_under_test = fparser.common.readfortran.FortranStringReader(code)
    mode = fparser.common.sourceinfo.FortranFormat(True, False)
    unit_under_test.set_format(mode)  # Force sloppy free format
    unit_under_test.get_source_item()
    assert log.messages['debug'] == []
    assert log.messages['info'] == []
    assert log.messages['warning'] == []
    assert log.messages['critical'] == []
    expected = 'following character continuation: \'"\', expected None.'
    result = log.messages['error'][0].split('<==')[1].lstrip()
    assert result == expected


def check_include_works(fortran_filename, fortran_code, include_info,
                        expected, tmpdir, ignore_comments=True):
    '''Utility function used by a number of tests to check that include
    files work as expected.

    :param str fortran_filename: the name of the fortran file that is \
    going to be created in the 'tmpdir' directory.
    :param str fortran_code: the fortran code to put in the fortran \
    file specified by 'fortran_filename'.
    :param include_info: a list of 2-tuples each with an include \
    filename as a string followed by include code as a string.
    :type include_info: list of (str, str)
    :param str expected: the expected output after parsing the code.
    :param str tmpdir: the temporary directory in which to create and \
    process the Fortran files.
    :param bool ignore_comments: whether to ignore (skip) comments in \
    the Fortran code or not. Defaults to ignore them.

    '''

    try:
        oldpwd = tmpdir.chdir()
        cwd = str(tmpdir)

        # Create the program
        with open(os.path.join(cwd, fortran_filename), "w") as cfile:
            cfile.write(fortran_code)
        for include_filename in include_info.keys():
            with open(os.path.join(cwd, include_filename), "w") as cfile:
                cfile.write(include_info[include_filename])
        reader = FortranFileReader(fortran_filename,
                                   ignore_comments=ignore_comments)
        for orig_line in expected.split("\n"):
            new_line = reader.next().line
            assert new_line == orig_line
        with pytest.raises(StopIteration):
            reader.next()
    finally:
        oldpwd.chdir()


FORTRAN_CODE = ("program test\n"
                "  ! prog comment 1\n"
                "  print *, 'Hello'\n"
                "  ! prog comment 2\n"
                "end program")

EXPECTED_CODE = ("program test\n"
                 "print *, 'Hello'\n"
                 "end program")


def test_include1(tmpdir):
    '''Test that FortranReaderBase can parse an include file when the
    original program consists only of an include.

    '''
    fortran_filename = "prog.f90"
    include_filename = "prog.inc"
    fortran_code = ("include '{0}'".format(include_filename))
    include_code = EXPECTED_CODE
    include_info = {include_filename: include_code}
    check_include_works(fortran_filename, fortran_code, include_info,
                        EXPECTED_CODE, tmpdir)


def test_include2(tmpdir):
    '''Test that FortranReaderBase can parse an include file when the
    original and include files both have multiple lines.

    '''
    fortran_filename = "prog.f90"
    include_filename = "my-include.h"
    fortran_code = ("module include_test\n"
                    "  include '{0}'\n"
                    "end module include_test".format(include_filename))
    include_code = ("interface mpi_sizeof\n"
                    "subroutine simple()\n"
                    "end subroutine simple\n"
                    "end interface mpi_sizeof")
    split_code = fortran_code.split("\n")
    expected = split_code[0] + "\n" + include_code + "\n" + split_code[2]
    include_info = {include_filename: include_code}
    check_include_works(fortran_filename, fortran_code, include_info,
                        expected, tmpdir)


def test_include3(tmpdir):
    '''Test that FortranReaderBase can parse an include file when the
    original program is invalid without the include.

    '''
    fortran_filename = "prog.f90"
    include_filename = "prog.inc"
    fortran_code = ("program test\n"
                    "include '{0}'".format(include_filename))
    include_code = ("print *, 'Hello'\n"
                    "end program")
    include_info = {include_filename: include_code}
    check_include_works(fortran_filename, fortran_code, include_info,
                        EXPECTED_CODE, tmpdir)


def test_include4(tmpdir):
    '''Test that FortranReaderBase can parse input containing multiple
    include files.

    '''
    fortran_filename = "prog.f90"
    include_filename1 = "prog.inc1"
    include_filename2 = "prog.inc2"
    fortran_code = ("program test\n"
                    "include '{0}'\n"
                    "include '{1}'".format(include_filename1,
                                           include_filename2))
    include_code1 = ("print *, 'Hello'\n")
    include_code2 = ("end program")
    expected = fortran_code.split("\n")[0] + "\n" + include_code1 + \
        include_code2
    include_info = {include_filename1: include_code1,
                    include_filename2: include_code2}
    check_include_works(fortran_filename, fortran_code, include_info,
                        expected, tmpdir)


def test_include5(tmpdir):
    '''Test that FortranReaderBase can parse nested include files.'''
    fortran_filename = "prog.f90"
    include_filename1 = "prog.inc1"
    include_filename2 = "prog.inc2"
    fortran_code = ("program test\n"
                    "include '{0}'".format(include_filename1))
    include_code1 = ("print *, 'Hello'\n"
                     "include '{0}'".format(include_filename2))
    include_code2 = ("end program")
    include_info = {include_filename1: include_code1,
                    include_filename2: include_code2}
    check_include_works(fortran_filename, fortran_code, include_info,
                        EXPECTED_CODE, tmpdir)


def test_include6(tmpdir, ignore_comments):
    '''Check that FortranReaderBase can parse an include file correctly
    when it contains comments. Test with and without comments being
    ignored.

    '''
    fortran_filename = "prog.f90"
    include_filename = "prog.inc"
    fortran_code = ("program test\n"
                    "  ! prog comment 1\n"
                    "  include '{0}'\n"
                    "  ! prog comment 2\n"
                    "end program".format(include_filename))
    include_code = ("! include comment 1\n"
                    "print *, 'Hello'\n"
                    "! include comment 2")
    include_info = {include_filename: include_code}
    if ignore_comments:
        expected = EXPECTED_CODE
    else:
        expected = ("program test\n"
                    "! prog comment 1\n"
                    "! include comment 1\n"
                    "print *, 'Hello'\n"
                    "! include comment 2\n"
                    "! prog comment 2\n"
                    "end program")
    check_include_works(fortran_filename, fortran_code, include_info,
                        expected, tmpdir, ignore_comments=ignore_comments)


def test_get_item(ignore_comments):
    '''Check the get_item() function works as expected. Test with and
    without comments being ignored.

    '''
    if ignore_comments:
        expected = EXPECTED_CODE
    else:
        expected = ("program test\n"
                    "! prog comment 1\n"
                    "print *, 'Hello'\n"
                    "! prog comment 2\n"
                    "end program")
    reader = FortranStringReader(FORTRAN_CODE, ignore_comments=ignore_comments)
    for expected_line in expected.split("\n"):
        output_line = reader.get_item()
        assert expected_line in output_line.line
    assert not reader.get_item()


def test_put_item(ignore_comments):
    '''Check that when a line is consumed it can be pushed back so it can
    be consumed again. Test with and without comments being
    ignored.

    '''
    reader = FortranStringReader(FORTRAN_CODE, ignore_comments=ignore_comments)
    while True:
        orig_line = reader.get_item()
        if not orig_line:
            break
        reader.put_item(orig_line)
        fifo_line = reader.get_item()
        assert fifo_line == orig_line


def test_put_item_include(ignore_comments):
    '''Check that when a line that has been included via an include
    statement is consumed it can be pushed back so it can be consumed
    again. Test with and without ignoring comments.

    '''
    reader = FortranStringReader(FORTRAN_CODE, ignore_comments=ignore_comments)
    while True:
        orig_line = reader.get_item()
        if not orig_line:
            break
        reader.put_item(orig_line)
        fifo_line = reader.get_item()
        assert fifo_line == orig_line


def test_multi_put_item(ignore_comments):
    '''Check that multiple lines can be pushed back and will be returned
    correctly in the specified order (actually the reverse of the
    original). Test with and without ignoring comments.

    '''
    reader = FortranStringReader(FORTRAN_CODE, ignore_comments=ignore_comments)
    orig_lines = []
    while True:
        orig_line = reader.get_item()
        if not orig_line:
            break
        # Make sure our original lines are kept in reverse order.
        orig_lines.insert(0, orig_line)

    # Put back original lines in reverse order as that is what we
    # would expect when processing and rolling back.
    for line in orig_lines:
        reader.put_item(line)

    # Lines should now be returned in the correct order (so compare in
    # reverse order with the original line list)
    while True:
        filo_line = reader.get_item()
        if not filo_line:
            break
        assert filo_line == orig_lines.pop(-1)
    assert not orig_lines

# Issue 177: get_item(ignore_comments) - how does ignore_comments affect
# processing?

# Issue 178: Why is there a next() as well as a get_item()? How do they
# (and put_item()) interact?


##############################################################################

FULL_FREE_SOURCE = u'''

!> Unicode comment: ❤ ✓ ☂ ♞ ☯

program test

  implicit none

  character, parameter :: nature = 'free format'

end program test
'''

FULL_FREE_EXPECTED = [u'!> Unicode comment: ❤ ✓ ☂ ♞ ☯',
                      'program test',
                      '  implicit none',
                      "  character, parameter :: nature = 'free format'",
                      'end program test']


##############################################################################

def test_filename_reader():
    '''
    Tests that a Fortran source file can be read given its filename.
    '''
    handle, filename = tempfile.mkstemp(suffix='.f90', text=True)
    os.close(handle)
    try:
        with io.open(filename, mode='w', encoding='UTF-8') as source_file:
            source_file.write(FULL_FREE_SOURCE)

        unit_under_test = FortranFileReader(filename)
        expected = fparser.common.sourceinfo.FortranFormat(True, False)
        assert unit_under_test.format == expected
        for expected in FULL_FREE_EXPECTED:
            found = unit_under_test.get_single_line(ignore_empty=True)
            assert found == expected
    except Exception:
        os.unlink(filename)
        raise


##############################################################################

def test_file_reader():
    '''
    Tests that a Fortran source file can be read given a file object of it.
    '''
    handle, filename = tempfile.mkstemp(suffix='.f90', text=True)
    os.close(handle)
    try:
        with io.open(filename, mode='w', encoding='UTF-8') as source_file:
            source_file.write(FULL_FREE_SOURCE)

        with io.open(filename, mode='r', encoding='UTF-8') as source_file:
            unit_under_test = FortranFileReader(source_file)

            expected = fparser.common.sourceinfo.FortranFormat(True, False)
            assert unit_under_test.format == expected
            for expected in FULL_FREE_EXPECTED:
                assert unit_under_test.get_single_line(ignore_empty=True) \
                       == expected
    except Exception:
        os.unlink(filename)
        raise


##############################################################################

def test_bad_file_reader():
    '''
    Tests that the file reader can spot when it is given something to read
    which is neither file nor filename.
    '''
    with pytest.raises(ValueError) as ex:
        unit_under_test = FortranFileReader(42)
    expected = 'FortranFileReader is used with a filename or file-like object.'
    assert expected in str(ex.value)


##############################################################################

def test_string_reader():
    '''
    Tests that Fortran source can be read from a string.
    '''
    unit_under_test = FortranStringReader(FULL_FREE_SOURCE)
    expected = fparser.common.sourceinfo.FortranFormat(True, False)
    assert unit_under_test.format == expected
    for expected in FULL_FREE_EXPECTED:
        assert unit_under_test.get_single_line(ignore_empty=True) == expected


##############################################################################

def test_inherited_f77():
    '''
    A grab bag of functional tests inherited from readfortran.py.
    '''
    string_f77 = """c -*- f77 -*-
c12346 comment
      subroutine foo
      call foo
     'bar
a    'g
      abc=2
cf2py call me ! hey
      call you ! hi
      end
     '"""
    expected = ["Comment('c -*- f77 -*-',(1, 1))",
                "Comment('c12346 comment',(2, 2))",
                "line #3'subroutine foo'",
                "line #4'call foobar'",
                'Comment("a    \'g",(6, 6))',
                "line #7'abc=2'",
                "line #9'call you ! hi'",
                "line #10'end'"]

    # Reading from buffer
    reader = fparser.common.readfortran.FortranStringReader(
        string_f77, ignore_comments=False)
    assert reader.format.mode == 'f77', repr(reader.format.mode)
    stack = expected[:]
    for item in reader:
        assert str(item) == stack.pop(0)

    # Reading from file
    handle, filename = tempfile.mkstemp(suffix='.f', text=True)
    os.close(handle)
    with open(filename, 'w') as fortran_file:
        print(string_f77, file=fortran_file)

    reader = fparser.common.readfortran.FortranFileReader(
        filename, ignore_comments=False)
    stack = expected[:]
    for item in reader:
        assert str(item) == stack.pop(0)


def test_pyf():
    '''
    Tests inherited from implementation.
    '''
    string_pyf = """! -*- pyf -*-
python module foo
  interface
  beginml '''1st line
  2nd line
  end line'''endml='tere!fake comment'!should be a comment
  a = 2
  'charc\"onstant' ''' single line mline '''a='hi!fake comment'!should be a comment
  a=\\\\\\\\\\'''not a multiline'''
  !blah='''never ending multiline
  b=3! hey, fake line continuation:&
  c=4& !line cont
  &45
  thisis_label_2 : c = 3
   xxif_isotropic_2 :     if ( string_upper_compare ( o%opt_aniso, 'ISOTROPIC' ) ) then
   g=3
   endif
  end interface
  if ( pc_get_lun() .ne. 6) &

    write ( pc_get_lun(), '( &
    & /, a, /, " p=", i4, " stopping c_flag=", a, &
    & /, " print unit=", i8)') &
    trim(title), pcpsx_i_pel(), trim(c_flag), pc_get_lun()
end python module foo
! end of file
"""
    expected = ["Comment('! -*- pyf -*-',(1, 1))",
                "line #2'python module foo'",
                "line #3'interface'",
                "MultiLine('  beginml ',"
                + "['1st line', '  2nd line', '  end line'],"
                + "\"endml='tere!fake comment'\",(4, 6))",
                "Comment('!should be a comment',(6, 6))",
                "line #7'a = 2'",
                "MultiLine('  \\'charc\"onstant\\' ',"
                + "[' single line mline '],"
                + "\"a='hi!fake comment'\",(8, 8))",
                "Comment('!should be a comment',(8, 8))",
                'line #9"a=\\\\\\\\\\\\\\\\\\\\\'\'\'not a mltiline\'\'\'"',
                'Comment("!blah=\'\'\'never ending mltiline",(10, 10))',
                "line #11'b=3'",
                "Comment('! hey, fake line continuation:&',(11, 11))",
                "line #12'c=445'",
                "Comment('!line cont',(12, 12))",
                "line #14thisis_label_2: 'c = 3'",
                'line #15xxif_isotropic_2: '
                + '"if ( string_upper_compare ( o%opt_aniso,'
                + ' \'ISOTROPIC\' ) ) then"',
                "line #16'g=3'",
                "line #17'endif'",
                "line #18'end interface'",
                'line #19\'if ( pc_get_lun() .ne. 6)'
                + '     write ( pc_get_lun(), \\\'(  /, a, /, " p=", i4,'
                + ' " stopping c_flag=", a,  /, " print unit=", i8)\\\')'
                + '     trim(title), pcpsx_i_pel(), trim(c_flag),'
                + ' pc_get_lun()\'',
                "line #25'end python module foo'",
                "Comment('! end of file',(26, 26))"]

    reader = fparser.common.readfortran.FortranStringReader(
        string_pyf, ignore_comments=False)
    assert reader.format.mode == 'pyf', repr(reader.format.mode)
    for item in reader:
        assert re.sub("u", "", str(item)) == re.sub("u", "", expected.pop(0))


def test_fix90():
    '''
    Tests inherited from implementation.
    '''
    string_fix90 = """c -*- fix -*-
      subroutine foo
cComment
 1234 a = 3 !inline comment
      b = 3
!
     !4!line cont. with comment symbol
     &5
      a = 3!f2py.14 ! pi!
!   KDMO
      write (obj%print_lun, *) ' KDMO : '
      write (obj%print_lun, *) '  COORD = ',coord, '  BIN_WID = ',             &
       obj%bin_wid,'  VEL_DMO = ', obj%vel_dmo
      end subroutine foo
      subroutine

     & foo
      end
"""
    expected = [u"Comment('c -*- fix -*-',(1, 1))",
                u"line #2'subroutine foo'",
                u"Comment('cComment',(3, 3))",
                u"line #4 1234 'a = 3'",
                u"Comment('!inline comment',(4, 4))",
                u"line #5'b = 345'",
                u"Comment('!',(6, 6))",
                u"Comment('!line cont. with comment symbol',(7, 7))",
                u"line #9'a = 3.14'",
                u"Comment('! pi!',(9, 9))",
                u"Comment('!   KDMO',(10, 10))",
                u'line #11"write (obj%print_lun, *) \' KDMO : \'"',
                u'line #12"write (obj%print_lun, *) \'  COORD = \',coord,'
                + u' \'  BIN_WID = \',             &"',
                u'line #13"obj%bin_wid,\'  VEL_DMO = \', obj%vel_dmo"',
                u"line #14'end subroutine foo'",
                u"line #15'subroutine foo'",
                u"Comment('',(16, 16))",
                u"line #18'end'"]
    reader = fparser.common.readfortran.FortranStringReader(
        string_fix90, ignore_comments=False)
    assert reader.format.mode == 'fix', repr(reader.format.mode)
    for item in reader:
        assert re.sub("u", "", six.text_type(item)) == \
            re.sub("u", "", expected.pop(0))


def test_utf_char_in_code(log):
    ''' Check that we cope with Fortran code that contains UTF characters. This
    is not valid Fortran but most compilers cope with it. '''
    log.reset()
    fort_file = os.path.join(os.path.dirname(__file__), "utf_in_code.f90")
    reader = FortranFileReader(fort_file,
                               ignore_comments=True)
    out_line = reader.get_item()
    while out_line:
        out_line = reader.get_item()
    assert log.messages['critical'] == []
