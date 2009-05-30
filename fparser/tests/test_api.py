"""
Test parsing of whole fortran files; 'blackbox' tests here.
"""

from fparser import api
import sys


def test_dimension_attr():
    source_str = '''
    subroutine foo
    integer, dimension( -10 : 10, -   2147483648 : 2147483648) :: a( -2 : 2, 1000000 : 1000001 )
    real, dimension(-20:20, 100:113, -  512  : 713) :: b
    end
    '''

    tree = api.parse(source_str, isfree=True, isstrict=False)
    subr = tree.a.external_subprogram['foo']
    avar = subr.a.variables['a']

    assert avar.dimension == [('-10', '10'), ('-   2147483648', '2147483648')]
    assert avar.bounds == [('-2', '2'), ('1000000', '1000001')]
    assert avar.shape == ['4', '1']

    bvar = subr.a.variables['b']

    assert bvar.dimension == [('-20', '20'), ('100', '113'), ('-  512', '713')]
    assert bvar.shape == ['40', '13', '1225']
