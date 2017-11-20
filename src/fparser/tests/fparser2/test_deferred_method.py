# -*- coding: utf-8 -*-
'''
Tests that the parser understands deferred methods.
'''
import fparser.api
import fparser.readfortran


def test_deferred_method():
    '''
    Tests that the parser understands deferred methods.
    '''
    source = '''! Abstract type
module abstract_test

  implicit none
  private

  type, abstract, public :: test_type
  contains
    procedure(method_interface), deferred :: method
  end type test_type

end module abstract_test
'''
    expected = '''
MODULE abstract_test
  IMPLICIT NONE
  PRIVATE
  TYPE, ABSTRACT, PUBLIC :: test_type
    CONTAINS
    PROCEDURE(method_interface), DEFERRED :: method
  END TYPE test_type
END MODULE abstract_test
'''.strip().split('\n')

    reader = fparser.readfortran.FortranStringReader(source)
    program_unit = fparser.api.Fortran2003.Program(reader)
    result = str(program_unit).strip().split('\n')

    assert result == expected
