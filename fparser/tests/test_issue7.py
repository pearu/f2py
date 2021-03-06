from __future__ import absolute_import

from fparser import api

def test_reproduce_issue():
    source_str = '''\
      subroutine bndfp()
      use m_struc_def
C-
C 
C
C
C
C
      end
'''
    tree = api.get_reader(source_str, isfree=False, isstrict=False)
    tree = list(tree)
    s, u, c, e = tree[:3]+tree[-1:]
    assert s.span==(1,1),repr(s.span)
    assert u.span==(2,2),repr(u.span)
    assert c.span==(3,3),repr(c.span)
    assert e.span==(9,9),repr(e.span)

def test_reproduce_issue_fix77():
    source_str = '''\
      subroutine foo()
      real a
c
c
      end
'''
    tree = api.get_reader(source_str, isfree=False, isstrict=True)
    tree = list(tree)
    foo, a, comment, end = tree[:3]+tree[-1:]
    assert foo.span==(1,1)
    assert a.span==(2,4),repr(a.span)
    assert comment.span==(5,5),repr(comment.span)
    assert end.span==(5,5),repr(end.span)

def test_reproduce_issue_fix90():
    source_str = '''\
      subroutine foo()
      real a
c 1
c 2
      end
'''
    tree = api.get_reader(source_str, isfree=False, isstrict=False)
    tree = list(tree)
    foo, a, comment,end = tree[:3]+tree[-1:]
    assert foo.span==(1,1)
    assert a.span==(2,2),repr(a.span)
    assert end.span==(5,5),repr(end.span)

    source_str = '''\
      subroutine foo()
      real a
c-
c
      end
'''
    tree = api.get_reader(source_str, isfree=False, isstrict=False)
    tree = list(tree)
    foo, a, comment,end = tree[:3]+tree[-1:]
    assert foo.span==(1,1)
    assert a.span==(2,2),repr(a.span)
    assert end.span==(5,5),repr(end.span)

    source_str = '''\
      subroutine foo()
      real a
c
c
      end
'''
    tree = api.get_reader(source_str, isfree=False, isstrict=False)
    tree = list(tree)
    foo, a, comment, end = tree[:3]+tree[-1:]
    assert foo.span==(1,1)
    assert a.span==(2,2),repr(a.span)
    assert comment.span == (3,3)
    assert end.span==(5,5),repr(end.span)

def test_comment_cont_fix90():
    source_str = '''\
          subroutine foo()
      real
c 1
     & a
c 2
      end
'''
    tree = api.get_reader(source_str, isfree=False, isstrict=False)
    tree = list(tree)
    foo, a, comment, end = tree[:3]+tree[-1:]
    assert foo.span==(1,1)
    assert a.span==(2,4),repr(a.span)
    assert comment.span==(3,3),repr(comment.span)
    assert end.span==(6,6)

    source_str = '''\
          subroutine foo()
      real
c
     & a
c 2
      end
'''
    tree = api.get_reader(source_str, isfree=False, isstrict=False)
    tree = list(tree)
    foo, a, comment, end = tree[:3]+tree[-1:]
    assert foo.span==(1,1)
    assert a.span==(2,4),repr(a.span)
    assert comment.span==(3,3),repr(comment.span)
    assert end.span==(6,6)

    source_str = '''\
          subroutine foo()
      real
c 1
     & a
c
      end
'''
    tree = api.get_reader(source_str, isfree=False, isstrict=False)
    tree = list(tree)
    foo, a, comment, end = tree[:3]+tree[-1:]
    assert foo.span==(1,1)
    assert a.span==(2,4),repr(a.span)
    assert comment.span==(3,3),repr(comment.span)
    assert end.span==(6,6)

    source_str = '''\
          subroutine foo()
      real
c 1
     & a
c 2
     &,b
      end
'''
    tree = api.get_reader(source_str, isfree=False, isstrict=False)
    tree = list(tree)
    foo, ab, comment, end = tree[:3]+tree[-1:]
    assert foo.span==(1,1)
    assert ab.span==(2,6),repr(a.span)
    assert comment.span==(3,3),repr(comment.span)
    assert end.span==(7,7)
