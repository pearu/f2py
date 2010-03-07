
from fparser import api

def test_reproduce_issue():
    source_str = '''
    module m
    contains
    subroutine a
    end subroutine a
    end module m
    '''
    tree = api.parse(source_str, isfree=True, isstrict=False)

def test_private_subroutine():
    source_str = '''
    module m
    public
    private a
    contains
    subroutine a
    end subroutine a
    subroutine b
    end subroutine b
    end module m
    '''
    tree = api.parse(source_str, isfree=True, isstrict=False)
    a = tree.content[0].content[3]
    b = tree.content[0].content[4]

    assert not a.is_public()
    assert a.is_private()

    assert b.is_public()
    assert not b.is_private()

