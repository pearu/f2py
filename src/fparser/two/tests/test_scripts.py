# Copyright (c) 2018-2019 Science and Technology Facilities Council
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

'''Test fparser scripts

'''

# pylint: disable=too-few-public-methods

import pytest
from fparser.scripts import fparser2, read

# fparser2.py script function runner()

# Create a dummy class (DummyArgs) with the required attribute to pass
# into runner() as an argument options class in subsequent tests


class DummyArgs(object):
    ''' dummy object pretending to be the argument options '''
    mode = "auto"
    task = "show"


def test_runner_no_files(capsys):
    '''Test that the script deals with no files provided as expected.'''
    # run the relevant script method (runner())
    with pytest.raises(SystemExit) as excinfo:
        fparser2.runner(None, DummyArgs(), [])
    assert str(excinfo.value) == "1"
    # capture the output and check that the appropriate error has been reported
    stdout, _ = capsys.readouterr()
    assert "Error: No fortran files specified" in stdout


def test_runner_non_existant_file(capsys):
    '''Test that the script reports an error when the file name that is
    provided does not exist.

    '''
    # run the relevant script method (runner())
    fparser2.runner(None, DummyArgs(), ["idontexist.txt"])
    # capture the output and check that the appropriate error has been reported
    stdout, _ = capsys.readouterr()
    assert "No such file or directory" in stdout


def test_runner_set_mode(tmpdir, capsys):
    '''Test that the script can change mode.'''
    # Create a temporary file containing Fortran code to pass into runner()
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    # Create a dummy class with the required attribute to pass into
    # runner() as an argument options class

    class DummyArgsFree(object):
        ''' dummy object pretending to be the argument options '''
        mode = "free"
        task = "show"
    # run the relevant script method (runner())
    fparser2.runner(None, DummyArgsFree(), [my_file.strpath])
    # capture the output and check that the code has been output
    stdout, _ = capsys.readouterr()
    assert "PROGRAM hello\nEND PROGRAM hello\n" in stdout


def test_runner_syntax_error(tmpdir, capsys):
    '''Test that the script deals with code with an invalid syntax.'''
    # Create a temporary file containing Fortran code to pass into runner()
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("prog error\nend program error\n")
    # run the relevant script method (runner())
    with pytest.raises(SystemExit) as excinfo:
        fparser2.runner(None, DummyArgs(), [my_file.strpath])
    assert str(excinfo.value) == "1"
    # capture the output and check that the appropriate error has been
    # reported
    stdout, _ = capsys.readouterr()
    assert "Syntax error: at line 1\n>>>prog error" in stdout
    assert "failed at line #1'prog error'" in stdout


def test_runner_syntax_error_2(tmpdir, capsys):
    '''Test that the script deals with code with an invalid syntax and
    where there is no information in the fifo buffer. I'm not sure why
    this happens but this checks the associated code that handles it
    works.

    '''
    # Create a temporary file containing Fortran code to pass into runner()
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program error\nif (.true.) then\nend if label\n"
                  "end program error\n")
    # run the relevant script method (runner())
    with pytest.raises(SystemExit) as excinfo:
        fparser2.runner(None, DummyArgs(), [my_file.strpath])
    assert str(excinfo.value) == "1"
    # capture the output and check that the appropriate error has been reported
    # There should be no file information (output by the script)
    stdout, _ = capsys.readouterr()
    assert (stdout == "Syntax error: at line 3\n>>>end if label\nName "
            "'label' has no corresponding starting name\n")


def test_runner_internal_error(tmpdir, monkeypatch, capsys):
    '''Test that the script deals with an internal error as expected.'''
    # Create a temporary file containing Fortran code to pass into runner()
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    # Create a dummy function that replaces the parser
    error_string = "monkey trouble"

    def dummy_parser(_):
        ''' dummy function that simply raises an internal error '''
        raise InternalError(error_string)
    # monkeypatch the parser so that it returns an InternalError exception.
    from fparser.two.parser import ParserFactory
    from fparser.two.utils import InternalError
    monkeypatch.setattr(ParserFactory, "create", dummy_parser)
    # run the relevant script method (runner())
    with pytest.raises(SystemExit) as excinfo:
        fparser2.runner(None, DummyArgs(), [my_file.strpath])
    assert str(excinfo.value) == "1"
    # capture the output and check that the appropriate error has been reported
    stdout, _ = capsys.readouterr()
    assert "Internal error in fparser: '{0}'".format(error_string) in stdout


def test_runner_output_task_show(tmpdir, capsys):
    '''Test that the script outputs the code it has parsed with the
    'task' option set to "show".

    '''
    class DummyArgsTask(object):
        ''' dummy object pretending to be the argument options '''
        mode = "free"
        task = "show"

    # Create a temporary file containing Fortran code to pass into
    # runner().
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    # Run the relevant script method (runner()).
    fparser2.runner(None, DummyArgsTask(), [my_file.strpath])
    # Capture the output and check that the code has been output.
    stdout, _ = capsys.readouterr()
    assert "PROGRAM hello\nEND PROGRAM hello\n" in stdout


def test_runner_output_task_repr(tmpdir, capsys):
    '''Test that the script outputs the repr representation of the code it
    has parsed with the 'task' option set to "repr".

    '''
    class DummyArgsTask(object):
        ''' dummy object pretending to be the argument options '''
        mode = "free"
        task = "repr"

    # Create a temporary file containing Fortran code to pass into
    # runner().
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    # Run the relevant script method (runner()).
    fparser2.runner(None, DummyArgsTask(), [my_file.strpath])
    # Capture the output and check that the repr of the code has been
    # produced.
    stdout, _ = capsys.readouterr()
    assert (stdout == "Program(Main_Program(Program_Stmt('PROGRAM',"
            " Name('hello')), End_Program_Stmt('PROGRAM', Name('hello'))))\n")


def test_runner_output_task_none(tmpdir, capsys):
    '''Test that the script outputs nothing when the 'task' option is set
    to "none".

    '''
    class DummyArgsTask(object):
        ''' dummy object pretending to be the argument options '''
        mode = "free"
        task = "none"

    # Create a temporary file containing Fortran code to pass into
    # runner().
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    # Run the relevant script method (runner()).
    fparser2.runner(None, DummyArgsTask(), [my_file.strpath])
    # Capture the output and check that nothig has been produced.
    stdout, _ = capsys.readouterr()
    assert stdout == ""


def test_runner_multi_output(tmpdir, capsys):
    '''Test that the script outputs the code it has parsed when there are
    multiple files specified

    '''
    # Create a temporary file containing Fortran code to pass into runner()
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    # run the relevant script method (runner())
    fparser2.runner(None, DummyArgs(), [my_file.strpath, my_file.strpath])
    # capture the output and check that the code has been output
    stdout, _ = capsys.readouterr()
    assert ("PROGRAM hello\nEND PROGRAM hello\n"
            "PROGRAM hello\nEND PROGRAM hello\n") in stdout

# fparser2.py script function main()


def test_main_output_task_default(tmpdir, capsys, monkeypatch):
    '''Test that the script main() function outputs the code it has parsed
    by default.'''
    import sys
    # Create a temporary file containing Fortran code to pass into runner()
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    # Use monkeypatch to spoof the command-line argument
    monkeypatch.setattr(sys, "argv", ["fparser2", my_file.strpath])
    # run the relevant script method (main())
    fparser2.main()
    # capture the output and check that the code has been output
    stdout, _ = capsys.readouterr()
    assert stdout == "PROGRAM hello\nEND PROGRAM hello\n"


def test_main_output_task_show(tmpdir, capsys, monkeypatch):
    '''Test that the script main() function outputs the code it has parsed
    when --task=show.

    '''
    import sys
    # Create a temporary file containing Fortran code to pass into
    # runner().
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    # Use monkeypatch to spoof the command-line argument.
    monkeypatch.setattr(sys, "argv", ["fparser2", "--task=show",
                                      my_file.strpath])
    # Run the relevant script method (main()).
    fparser2.main()
    # Capture the output and check that the code has been output.
    stdout, _ = capsys.readouterr()
    assert stdout == "PROGRAM hello\nEND PROGRAM hello\n"


def test_main_output_task_repr(tmpdir, capsys, monkeypatch):
    '''Test that the script main() function outputs the 'repr' of the code
    it has parsed when --task=repr.

    '''
    import sys
    # Create a temporary file containing Fortran code to pass into
    # runner().
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    # Use monkeypatch to spoof the command-line argument.
    monkeypatch.setattr(sys, "argv", ["fparser2", "--task=repr",
                                      my_file.strpath])
    # Run the relevant script method (main()).
    fparser2.main()
    # Capture the output and check that the code has been output.
    stdout, _ = capsys.readouterr()
    assert stdout == ("Program(Main_Program(Program_Stmt('PROGRAM',"
                      " Name('hello')), End_Program_Stmt('PROGRAM',"
                      " Name('hello'))))\n")


def test_main_output_task_none(tmpdir, capsys, monkeypatch):
    '''Test that the script main() function outputs nothing when
    --task=none.

    '''
    import sys
    # Create a temporary file containing Fortran code to pass into
    # runner().
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    # Use monkeypatch to spoof the command-line argument.
    monkeypatch.setattr(sys, "argv", ["fparser2", "--task=none",
                                      my_file.strpath])
    # Run the relevant script method (main()).
    fparser2.main()
    # Capture the output and check that the code has been output.
    stdout, _ = capsys.readouterr()
    assert stdout == ""


def test_main_output_task_invalid(tmpdir, capsys, monkeypatch):
    '''Test that the script main() function prints an error when an
    invalid task option is provided.

    '''
    import sys
    # Create a temporary file containing Fortran code to pass into
    # runner().
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    # Use monkeypatch to spoof the command-line argument.
    monkeypatch.setattr(sys, "argv", ["fparser2", "--task=invalid",
                                      my_file.strpath])
    with pytest.raises(SystemExit):
        # Run the relevant script method (main()).
        fparser2.main()
    # Capture the output and check that the appropriate error has been
    # written.
    _, stderr = capsys.readouterr()
    assert ("fparser2: error: option --task: invalid choice: 'invalid'"
            "" in stderr)

# read.py script function runner()

# Create a dummy class (DummyReadArgs) with the required attribute to pass
# into runner() as an argument options class in subsequent tests


class DummyReadArgs(object):
    '''dummy object pretending to be the argument options for the read
    module in scripts.

    :param str task: the value to set the internal task
    variable. Defaults to "show".

    '''
    def __init__(self, task="show"):
        self.task = task


def test_read_runner_nofiles(capsys):
    '''Test that the script deals with no files provided as expected.'''
    # run the relevant script method (runner())
    read.runner(None, DummyArgs(), [])
    # capture the output and check that no output is generated
    stdout, _ = capsys.readouterr()
    assert stdout == ""


def test_read_runner_file(capsys, tmpdir):
    '''Test that the script deals with one file as expected.'''
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    read.runner(None, DummyReadArgs(), [str(my_file)])
    stdout, _ = capsys.readouterr()
    assert "line #1'program hello'" in stdout
    assert "line #2'end program hello'" in stdout


def test_read_runner_files(capsys, tmpdir):
    '''Test that the script deals with multiple files as expected.'''
    my_file1 = tmpdir.mkdir("sub1").join("hello1.f90")
    my_file1.write("program hello1\nend program hello1\n")
    my_file2 = tmpdir.mkdir("sub2").join("hello2.f90")
    my_file2.write("program hello2\nend program hello2\n")
    read.runner(None, DummyReadArgs(), [str(my_file1), str(my_file2)])
    stdout, _ = capsys.readouterr()
    assert "line #1'program hello1'" in stdout
    assert "line #2'end program hello1'" in stdout
    assert "line #1'program hello2'" in stdout
    assert "line #2'end program hello2'" in stdout


def test_read_runner_no_show(tmpdir):
    '''Test that the script raises an exception if the task argument !=
    "show" as that is the only option currently supported.

    '''
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    with pytest.raises(NotImplementedError) as excinfo:
        read.runner(None, DummyReadArgs("invalid"), [str(my_file)])
    assert ("The task option ''invalid'' is invalid. Currently only 'show' "
            "is supported.") in str(excinfo.value)


def test_read_main(capsys, tmpdir, monkeypatch):
    '''Test that the main function works as expected.'''
    import sys
    # Create a temporary file containing Fortran code.
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    # Use monkeypatch to spoof the command-line argument
    monkeypatch.setattr(sys, "argv", ["read", str(my_file)])
    # run the relevant script method (main())
    read.main()
    # capture the output and check that the code has been output
    stdout, _ = capsys.readouterr()
    assert "line #1'program hello'" in stdout
    assert "line #2'end program hello'" in stdout
