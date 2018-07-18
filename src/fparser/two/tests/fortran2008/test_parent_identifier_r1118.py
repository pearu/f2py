'''Test Fortran 2008 rule R1118 (C1113)

   parent-identifier is ancestor-module-name [ : parent-submodule-name ]

   C1113 The ancestor-module-name shall be the name of a nonintrinsic
   module; the parent-submodule name shall be the name of a
   descendant of that module.

   It is not possible to test C1113 with fparser as the names may be
   in different files

'''
import pytest
from fparser.two.Fortran2003 import NoMatchError
from fparser.two.Fortran2008 import Parent_Identifier
from fparser.two.parser import ParserFactory
# this is required to setup the fortran2008 classes
_ = ParserFactory().create(std="f2008")


def test_simple_1():
    '''Test the parsing of a minimal parent-identifier statement'''
    result = Parent_Identifier("modulename")
    assert str(result) == "modulename"


def test_simple_2():
    '''Test the parsing of a minimal parent-identifier statement'''
    result = Parent_Identifier("modulename : submodulename")
    assert str(result) == "modulename:submodulename"


def test_simple_error1():
    '''Test a parent_identifier statement with a syntax error raises an
    exception

    '''
    with pytest.raises(NoMatchError) as excinfo:
        result = Parent_Identifier("modulename ; submodulename")
    assert "Parent_Identifier: 'modulename ; submodulename'" in str(excinfo.value)


def test_simple_error2():
    '''Test a parent_identifier statement with a syntax error raises an
    exception

    '''
    with pytest.raises(NoMatchError) as excinfo:
        _ = Parent_Identifier("modulename :")
    assert "Parent_Identifier: 'modulename :'" in str(excinfo.value)


def test_simple_error3():
    '''Test a parent_identifier statement with a syntax error raises an
    exception

    '''
    with pytest.raises(NoMatchError) as excinfo:
        _ = Parent_Identifier(": submodulename")
    assert "Parent_Identifier: ': submodulename'" in str(excinfo.value)


@pytest.mark.xfail(reason="Does not raise an error when there are too "
                   "many colons")
def test_simple_error4():
    '''Test a parent_identifier statement with a syntax error raises an
    exception

    '''
    with pytest.raises(NoMatchError) as excinfo:
        _ = Parent_Identifier("modulename : submodulename : anothername")
    assert ("Parent_Identifier: 'modulename : submodulename : anothername'"
            in str(excinfo.value))



@pytest.mark.xfail(reason="Does not raise an error if there is extra "
                   "content after the match")
def test_simple_error5():
    '''Test a parent-identifier statement with additional content after
    the match raises an exception

    '''
    with pytest.raises(NoMatchError) as excinfo:
        _ = Parent_Identifier("modulename : submodulename :")
    assert ("Parent_Identifier: 'modulename : submodulename extra'"
            in str(excinfo.value))
