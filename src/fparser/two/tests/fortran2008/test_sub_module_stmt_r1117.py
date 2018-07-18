'''Test Fortran 2008 rule R1117

    submodule-stmt is SUBMODULE ( parent-identifier ) submodule-name

'''
import pytest
from fparser.two.Fortran2003 import NoMatchError
from fparser.two.Fortran2008 import Sub_Module_Stmt
from fparser.two.parser import ParserFactory
# this is required to setup the fortran2008 classes
_ = ParserFactory().create(std="f2008")


def test_simple():
    ''' Test the parsing of a submodule statement'''
    result = Sub_Module_Stmt("submodule (id) name")
    assert str(result) == "SUBMODULE (id) name"


def test_simple_error1():
    ''' Test the parsing of a submodule statement'''
    with pytest.raises(NoMatchError) as excinfo:
        _ = Sub_Module_Stmt("submod (id) name")
    assert "Sub_Module_Stmt: 'submod (id) name" in str(excinfo.value)


def test_simple_error2():
    ''' Test the parsing of a submodule statement'''
    with pytest.raises(NoMatchError) as excinfo:
        _ = Sub_Module_Stmt("submodule name")
    assert "Sub_Module_Stmt: 'submodule name'" in str(excinfo.value)


def test_simple_error3():
    ''' Test the parsing of a submodule statement'''
    with pytest.raises(NoMatchError) as excinfo:
        _ = Sub_Module_Stmt("submodule () name")
    assert "Sub_Module_Stmt: 'submodule () name'" in str(excinfo.value)


def test_simple_error4():
    ''' Test the parsing of a submodule statement'''
    with pytest.raises(NoMatchError) as excinfo:
        _ = Sub_Module_Stmt("submodule (id)")
    assert "Sub_Module_Stmt: 'submodule (id)'" in str(excinfo.value)


def test_simple_error5():
    ''' Test the parsing of a submodule statement'''
    with pytest.raises(NoMatchError) as excinfo:
        _ = Sub_Module_Stmt("submodule name (id)")
    assert "Sub_Module_Stmt: 'submodule name (id)'" in str(excinfo.value)


def test_simple_error6():
    ''' Test the parsing of a submodule statement'''
    with pytest.raises(NoMatchError) as excinfo:
        _ = Sub_Module_Stmt("submodule (id) (name)")
    assert "Sub_Module_Stmt: 'submodule (id) (name)'" in str(excinfo.value)


@pytest.mark.xfail(reason="Does not raise an error if there is extra "
                   "content after the match")
def test_simple_error7():
    ''' Test the parsing of a submodule statement'''
    with pytest.raises(NoMatchError) as excinfo:
        _ = Sub_Module_Stmt("submodule (id) name :")
    assert "Sub_Module_Stmt: 'submodule (id) name name2'" in str(excinfo.value)

