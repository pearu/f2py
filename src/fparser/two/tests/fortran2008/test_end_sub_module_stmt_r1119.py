'''Test Fortran 2008 rule R1119

    end-submodule-stmt is END [ SUBMODULE [ submodule-name ] ]

'''
import pytest
from fparser.two.Fortran2003 import NoMatchError
from fparser.two.Fortran2008 import End_Sub_Module_Stmt
from fparser.two.parser import ParserFactory
# this is required to setup the fortran2008 classes
_ = ParserFactory().create(std="f2008")


def test_simple_1():
    '''Test the parsing of a minimal end-submodule statement'''
    result = End_Sub_Module_Stmt("end")
    assert str(result) == "END SUBMODULE"


def test_simple_2():
    '''Test the parsing of an end-submodule statement which includes the
    submodule keyword

    '''
    result = End_Sub_Module_Stmt("end submodule")
    assert str(result) == "END SUBMODULE"


def test_simple_3():
    '''Test the parsing of an end-submodule statement which includes the
    submodule name

    '''
    result = End_Sub_Module_Stmt("end submodule name")
    assert str(result) == "END SUBMODULE name"


def test_simple_error1():
    '''Test an end-submodule statement with a syntax error raises an
    exception

    '''
    with pytest.raises(NoMatchError) as excinfo:
        _ = End_Sub_Module_Stmt("edn")
    assert "End_Sub_Module_Stmt: 'edn'" in str(excinfo.value)


def test_simple_error2():
    '''Test an end-submodule statement with a syntax error raises an
    exception

    '''
    with pytest.raises(NoMatchError) as excinfo:
        _ = End_Sub_Module_Stmt("end submod")
    assert "End_Sub_Module_Stmt: 'end submod'" in str(excinfo.value)


@pytest.mark.xfail(reason="Does not raise an error if there is extra "
                   "content after the match")
def test_simple_error3():
    '''Test an end-submodule statement with additional content after the
    match raises an exception

    '''
    with pytest.raises(NoMatchError) as excinfo:
        _ = End_Sub_Module_Stmt("end submodule name :")
    assert "End_Sub_Module_Stmt: 'submodule () name'" in str(excinfo.value)
