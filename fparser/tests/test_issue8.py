
from fparser import api

def test_reproduce_issue():
    source_str = '''\
      subroutine gwinput_v2x(ifi,konf,ncore)
      integer :: ifi, !File handle. Write
     &  konf(0:lmxamx,nclass),! Principle
                              ! For examp
                              ! Core orbi
                              !   1, 2,..
                              !   2, 3,..
                              !   3, 4,..
                              !
     &  ncore(nclass)   ! ncore = \sum_l 
                        ! Number of diffe
      end
'''
    tree = api.parse(source_str, isfree=False, isstrict=False,
                     ignore_comments=False)
    assert str(tree).strip().split('\n')[1:]=='''
      !      BEGINSOURCE <cStringIO.StringI object at 0x1e52ea0> mode=fix90
        SUBROUTINE gwinput_v2x(ifi, konf, ncore)
          INTEGER ifi, konf(0:lmxamx,nclass), ncore(nclass)
          !File handle. Write
          ! Principle
          ! For examp
          ! Core orbi
          !   1, 2,..
          !   2, 3,..
          !   3, 4,..
          !
          ! ncore = \sum_l
          ! Number of diffe
        END SUBROUTINE gwinput_v2x
    '''.strip().split('\n')[1:]
