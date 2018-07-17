'''Test Fortran 2008 rule R202 : This file tests the addition of
submodules in Fortran2008 for the Program-unit rule

'''

import pytest
from fparser.two.Fortran2003 import NoMatchError
from fparser.api import get_reader
from fparser.two.Fortran2008 import Program_Unit
from fparser.two.parser import ParserFactory
# this is required to setup the Fortran2008 classes
_ = ParserFactory().create(std="f2008")


def test_other():
    '''Test that something other than submodule can still be parsed
    i.e. by adding submodule we've not broken the existing
    program-unit options

    '''
    reader = get_reader('''\
      subroutine test()
      end subroutine
      ''')
    ast = Program_Unit(reader)
    assert "SUBROUTINE test\n" \
        "END SUBROUTINE" in str(ast)


def test_submodule():
    '''Test that submodule as a top-level program unit can be parsed'''
    reader = get_reader('''\
      submodule (foobar) bar
      end
      ''')
    ast = Program_Unit(reader)
    assert "SUBMODULE (foobar) bar\n" \
        "END SUBMODULE bar" in str(ast)


def test_submodule_nomatch():
    '''Test an exception is raised if there is a syntax error'''
    reader = get_reader('''\
      submod (foobar) bar
      end
      ''')
    with pytest.raises(NoMatchError) as excinfo:
        _ = Program_Unit(reader)
    assert ("at line 1\n>>>      submod (foobar) bar\n"
            in str(excinfo.value))
