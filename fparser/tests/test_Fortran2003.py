from fparser.Fortran2003 import *
from fparser.api import get_reader

from nose.tools import assert_equal

def assertRaises(exc, cls, s):
    try:
        cls(s)
        raise AssertionError('Expected %s but got nothing' % exc)
    except exc:
        pass


###############################################################################
############################### SECTION  2 ####################################
###############################################################################

def test_Program(): # R201

        cls = Program
        reader = get_reader('''\
      subroutine foo
      end subroutine foo
      subroutine bar
      end
      ''')
        a = cls(reader)
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'SUBROUTINE foo\nEND SUBROUTINE foo\nSUBROUTINE bar\nEND SUBROUTINE bar')

        reader = get_reader('''\
      subroutine foo (*)
      end subroutine foo
      ''')
        a = cls(reader)
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'SUBROUTINE foo(*)\nEND SUBROUTINE foo')

def test_Specification_Part(): # R204

        reader = get_reader('''\
      integer a''')
        cls = Specification_Part
        a = cls(reader)
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'INTEGER :: a')
        assert_equal(repr(a), "Specification_Part(Type_Declaration_Stmt(Intrinsic_Type_Spec('INTEGER', None), None, Entity_Decl(Name('a'), None, None, None)))")

###############################################################################
############################### SECTION  3 ####################################
###############################################################################

def test_Name(): # R304

        a = Name('a')
        assert isinstance(a,Name),`a`
        a = Name('a2')
        assert isinstance(a,Name),`a`
        a = Designator('a')
        assert isinstance(a,Name),`a`
        a = Constant('a')
        assert isinstance(a,Name),`a`
        a = Expr('a')
        assert isinstance(a,Name),`a`

def test_Literal_Constant(): # R305

    cls = Constant
    a = cls('.false.')
    assert isinstance(a, Logical_Literal_Constant), `a`
    assert str(a)=='.FALSE.'

def test_Literal_Constant(): # R306

    cls = Literal_Constant
    a = cls('.false.')
    assert isinstance(a, Logical_Literal_Constant), `a`
    assert str(a)=='.FALSE.'

###############################################################################
############################### SECTION  4 ####################################
###############################################################################

def test_Type_Param_Value(): # 402

        cls = Type_Param_Value
        a = cls('*')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'*')
        assert_equal(repr(a),"Type_Param_Value('*')")

        a = cls(':')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),':')

        a = cls('1+2')
        assert isinstance(a,Level_2_Expr),`a`
        assert_equal(str(a),'1 + 2')

def test_Intrinsic_Type_Spec(): # R403

        cls = Intrinsic_Type_Spec
        a = cls('INTEGER')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'INTEGER')
        assert_equal(repr(a), "Intrinsic_Type_Spec('INTEGER', None)")

        a = cls('Integer*2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'INTEGER*2')

        a = cls('real*2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'REAL*2')

        a = cls('logical*2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'LOGICAL*2')

        a = cls('complex*2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'COMPLEX*2')

        a = cls('character*2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'CHARACTER*2')

        a = cls('double complex')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'DOUBLE COMPLEX')

        a = cls('double  precision')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'DOUBLE PRECISION')

def test_Kind_Selector(): # R404

        cls = Kind_Selector
        a = cls('(1)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(KIND = 1)')
        assert_equal(repr(a),"Kind_Selector('(', Int_Literal_Constant('1', None), ')')")

        a = cls('(kind=1+2)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(KIND = 1 + 2)')

        a = cls('* 1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'*1')

def test_Signed_Int_Literal_Constant(): # R405

        cls = Signed_Int_Literal_Constant
        a = cls('1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1')
        assert_equal(repr(a),"%s('1', None)" % (cls.__name__))

        a = cls('+ 21_2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+ 21_2')
        assert_equal(repr(a),"%s('+ 21', '2')" % (cls.__name__))

        a = cls('-21_SHORT')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'-21_SHORT')

        a = cls('21_short')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'21_short')

        a = cls('+1976354279568241_8')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+1976354279568241_8')

def test_Int_Literal_Constant(): # R406

        cls = Int_Literal_Constant
        a = cls('1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1')
        assert_equal(repr(a),"%s('1', None)" % (cls.__name__))

        a = cls('21_2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'21_2')
        assert_equal(repr(a),"%s('21', '2')" % (cls.__name__))

        a = cls('21_SHORT')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'21_SHORT')

        a = cls('21_short')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'21_short')

        a = cls('1976354279568241_8')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1976354279568241_8')

def test_Binary_Constant(): # R412

        cls = Boz_Literal_Constant
        bcls = Binary_Constant
        a = cls('B"01"')
        assert isinstance(a,bcls),`a`
        assert_equal(str(a),'B"01"')
        assert_equal(repr(a),"%s('B\"01\"')" % (bcls.__name__))

def test_Octal_Constant(): # R413

        cls = Boz_Literal_Constant
        ocls = Octal_Constant
        a = cls('O"017"')
        assert isinstance(a,ocls),`a`
        assert_equal(str(a),'O"017"')
        assert_equal(repr(a),"%s('O\"017\"')" % (ocls.__name__))

def test_Hex_Constant(): # R414

        cls = Boz_Literal_Constant
        zcls = Hex_Constant
        a = cls('Z"01A"')
        assert isinstance(a,zcls),`a`
        assert_equal(str(a),'Z"01A"')
        assert_equal(repr(a),"%s('Z\"01A\"')" % (zcls.__name__))

def test_Signed_Real_Literal_Constant(): # R416

        cls = Signed_Real_Literal_Constant
        a = cls('12.78')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'12.78')
        assert_equal(repr(a),"%s('12.78', None)" % (cls.__name__))

        a = cls('+12.78_8')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+12.78_8')
        assert_equal(repr(a),"%s('+12.78', '8')" % (cls.__name__))

        a = cls('- 12.')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'- 12.')

        a = cls('1.6E3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6E3')

        a = cls('+1.6E3_8')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+1.6E3_8')

        a = cls('1.6D3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6D3')

        a = cls('-1.6E-3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'-1.6E-3')
        a = cls('1.6E+3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6E+3')

        a = cls('3E4')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'3E4')

        a = cls('.123')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.123')

        a = cls('+1.6E-3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+1.6E-3')

        a = cls('10.9E7_QUAD')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'10.9E7_QUAD')

        a = cls('-10.9e-17_quad')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'-10.9E-17_quad')

def test_Real_Literal_Constant(): # R417

        cls = Real_Literal_Constant
        a = cls('12.78')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'12.78')
        assert_equal(repr(a),"%s('12.78', None)" % (cls.__name__))

        a = cls('12.78_8')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'12.78_8')
        assert_equal(repr(a),"%s('12.78', '8')" % (cls.__name__))

        a = cls('12.')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'12.')

        a = cls('1.6E3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6E3')

        a = cls('1.6E3_8')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6E3_8')

        a = cls('1.6D3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6D3')

        a = cls('1.6E-3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6E-3')
        a = cls('1.6E+3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6E+3')

        a = cls('3E4')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'3E4')

        a = cls('.123')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.123')

        a = cls('1.6E-3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6E-3')

        a = cls('10.9E7_QUAD')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'10.9E7_QUAD')

        a = cls('10.9e-17_quad')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'10.9E-17_quad')

        a = cls('0.0D+0')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'0.0D+0')

def test_Char_Selector(): # R424

        cls = Char_Selector
        a = cls('(len=2, kind=8)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(LEN = 2, KIND = 8)')
        assert_equal(repr(a),"Char_Selector(Int_Literal_Constant('2', None), Int_Literal_Constant('8', None))")


        a = cls('(2, kind=8)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(LEN = 2, KIND = 8)')

        a = cls('(2, 8)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(LEN = 2, KIND = 8)')

        a = cls('(kind=8)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(KIND = 8)')

        a = cls('(kind=8,len=2)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(LEN = 2, KIND = 8)')

def test_Complex_Literal_Constant(): # R421

        cls = Complex_Literal_Constant
        a = cls('(1.0, -1.0)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(1.0, -1.0)')
        assert_equal(repr(a),"Complex_Literal_Constant(Signed_Real_Literal_Constant('1.0', None), Signed_Real_Literal_Constant('-1.0', None))")

        a = cls('(3,3.1E6)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(3, 3.1E6)')

        a = cls('(4.0_4, 3.6E7_8)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(4.0_4, 3.6E7_8)')

        a = cls('( 0., PI)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(0., PI)')


def test_Type_Name(): # C424

        cls = Type_Name
        a = cls('a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a')
        assert_equal(repr(a),"Type_Name('a')")

        assertRaises(NoMatchError,cls,'integer')
        assertRaises(NoMatchError,cls,'doubleprecision')

def test_Length_Selector(): # R425

        cls = Length_Selector
        a = cls('( len = *)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(LEN = *)')
        assert_equal(repr(a),"Length_Selector('(', Type_Param_Value('*'), ')')")

        a = cls('*2,')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'*2')

def test_Char_Length(): # R426

        cls = Char_Length
        a = cls('(1)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(1)')
        assert_equal(repr(a),"Char_Length('(', Int_Literal_Constant('1', None), ')')")

        a = cls('1')
        assert isinstance(a,Int_Literal_Constant),`a`
        assert_equal(str(a),'1')

        a = cls('(*)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(*)')

        a = cls('(:)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(:)')

def test_Char_Literal_Constant(): # R427

        cls = Char_Literal_Constant
        a = cls('NIH_"DO"')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'NIH_"DO"')
        assert_equal(repr(a),'Char_Literal_Constant(\'"DO"\', \'NIH\')')

        a = cls("'DO'")
        assert isinstance(a,cls),`a`
        assert_equal(str(a),"'DO'")
        assert_equal(repr(a),'Char_Literal_Constant("\'DO\'", None)')

        a = cls("'DON''T'")
        assert isinstance(a,cls),`a`
        assert_equal(str(a),"'DON''T'")

        a = cls('"DON\'T"')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'"DON\'T"')

        a = cls('""')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'""')

        a = cls("''")
        assert isinstance(a,cls),`a`
        assert_equal(str(a),"''")

        a = cls('"hey ha(ada)\t"')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'"hey ha(ada)\t"')

def test_Logical_Literal_Constant(): # R428

        cls = Logical_Literal_Constant
        a = cls('.TRUE.')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.TRUE.')
        assert_equal(repr(a),"%s('.TRUE.', None)" % (cls.__name__))

        a = cls('.True.')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.TRUE.')

        a = cls('.FALSE.')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.FALSE.')
        
        a = cls('.false.')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.FALSE.')

        a = cls('.TRUE._HA')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.TRUE._HA')

def test_Derived_Type_Stmt(): # R430

        cls = Derived_Type_Stmt
        a = cls('type a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'TYPE :: a')
        assert_equal(repr(a),"Derived_Type_Stmt(None, Type_Name('a'), None)")

        a = cls('type ::a(b,c)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'TYPE :: a(b, c)')

        a = cls('type, private, abstract::a(b,c)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'TYPE, PRIVATE, ABSTRACT :: a(b, c)')

def test_Type_Name(): # C423

        cls = Type_Name
        a = cls('a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a')
        assert_equal(repr(a),"Type_Name('a')")

def test_Type_Attr_Spec(): # R431

        cls = Type_Attr_Spec
        a = cls('abstract')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'ABSTRACT')
        assert_equal(repr(a),"Type_Attr_Spec('ABSTRACT')")

        a = cls('bind (c )')
        assert isinstance(a, Language_Binding_Spec),`a`
        assert_equal(str(a),'BIND(C)')

        a = cls('extends(a)')
        assert isinstance(a, Type_EXTENDS_Parent_Type_Name),`a`
        assert_equal(str(a),'EXTENDS(a)')

        a = cls('private')
        assert isinstance(a, Access_Spec),`a`
        assert_equal(str(a),'PRIVATE')


def test_End_Type_Stmt(): # R433

        cls = End_Type_Stmt
        a = cls('end type')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'END TYPE')
        assert_equal(repr(a),"End_Type_Stmt('TYPE', None)")

        a = cls('end type  a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'END TYPE a')

def test_Sequence_Stmt(): # R434

        cls = Sequence_Stmt
        a = cls('sequence')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SEQUENCE')
        assert_equal(repr(a),"Sequence_Stmt('SEQUENCE')")

def test_Type_Param_Def_Stmt(): # R435

        cls = Type_Param_Def_Stmt
        a = cls('integer ,kind :: a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'INTEGER, KIND :: a')
        assert_equal(repr(a),"Type_Param_Def_Stmt(None, Type_Param_Attr_Spec('KIND'), Name('a'))")

        a = cls('integer*2 ,len :: a=3, b=2+c')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'INTEGER*2, LEN :: a = 3, b = 2 + c')

def test_Type_Param_Decl(): # R436

        cls = Type_Param_Decl
        a = cls('a=2')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a = 2')
        assert_equal(repr(a),"Type_Param_Decl(Name('a'), '=', Int_Literal_Constant('2', None))")

        a = cls('a')
        assert isinstance(a, Name),`a`
        assert_equal(str(a),'a')

def test_Type_Param_Attr_Spec(): # R437

        cls = Type_Param_Attr_Spec
        a = cls('kind')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'KIND')
        assert_equal(repr(a),"Type_Param_Attr_Spec('KIND')")

        a = cls('len')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'LEN')

def test_Component_Attr_Spec(): # R441

        cls = Component_Attr_Spec
        a = cls('pointer')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'POINTER')
        assert_equal(repr(a),"Component_Attr_Spec('POINTER')")

        a = cls('allocatable')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'ALLOCATABLE')

        a = cls('dimension(a)')
        assert isinstance(a, Dimension_Component_Attr_Spec),`a`
        assert_equal(str(a),'DIMENSION(a)')

        a = cls('private')
        assert isinstance(a, Access_Spec),`a`
        assert_equal(str(a),'PRIVATE')

def test_Component_Decl(): # R442

        cls = Component_Decl
        a = cls('a(1)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(1)')
        assert_equal(repr(a),"Component_Decl(Name('a'), Explicit_Shape_Spec(None, Int_Literal_Constant('1', None)), None, None)")

        a = cls('a(1)*(3)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(1)*(3)')

        a = cls('a(1)*(3) = 2')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(1)*(3) = 2')

        a = cls('a(1) => NULL')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(1) => NULL')

def test_Final_Binding(): # R454

        cls = Final_Binding
        a = cls('final a, b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'FINAL :: a, b')
        assert_equal(repr(a),"Final_Binding('FINAL', Final_Subroutine_Name_List(',', (Name('a'), Name('b'))))")

        a = cls('final::a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'FINAL :: a')

def test_Derived_Type_Spec(): # R455

        cls = Derived_Type_Spec
        a = cls('a(b)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a(b)')
        assert_equal(repr(a),"Derived_Type_Spec(Type_Name('a'), Name('b'))")

        a = cls('a(b,c,g=1)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a(b, c, g = 1)')

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

        a = cls('a()')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a()')

def test_Type_Param_Spec(): # R456

        cls = Type_Param_Spec
        a = cls('a=1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a = 1')
        assert_equal(repr(a),"Type_Param_Spec(Name('a'), Int_Literal_Constant('1', None))")

        a = cls('k=a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = a')

        a = cls('k=:')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = :')

def test_Type_Param_Spec_List(): # R456-list

        cls = Type_Param_Spec_List

        a = cls('a,b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a, b')
        assert_equal(repr(a),"Type_Param_Spec_List(',', (Name('a'), Name('b')))")

        a = cls('a')
        assert isinstance(a,Name),`a`

        a = cls('k=a,c,g=1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = a, c, g = 1')

def test_Structure_Constructor_2(): # R457.b

        cls = Structure_Constructor_2
        a = cls('k=a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = a')
        assert_equal(repr(a),"Structure_Constructor_2(Name('k'), Name('a'))")

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

def test_Structure_Constructor(): # R457

        cls = Structure_Constructor
        a = cls('t()')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'t()')
        assert_equal(repr(a),"Structure_Constructor(Type_Name('t'), None)")

        a = cls('t(s=1, a)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'t(s = 1, a)')

        a = cls('a=k')
        assert isinstance(a,Structure_Constructor_2),`a`
        assert_equal(str(a),'a = k')
        assert_equal(repr(a),"Structure_Constructor_2(Name('a'), Name('k'))")

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

def test_Component_Spec(): # R458

        cls = Component_Spec
        a = cls('k=a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = a')
        assert_equal(repr(a),"Component_Spec(Name('k'), Name('a'))")

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

        a = cls('a % b')
        assert isinstance(a, Proc_Component_Ref),`a`
        assert_equal(str(a),'a % b')

        a = cls('s =a % b')
        assert isinstance(a, Component_Spec),`a`
        assert_equal(str(a),'s = a % b')

def test_Component_Spec_List(): # R458-list

        cls = Component_Spec_List
        a = cls('k=a, b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = a, b')
        assert_equal(repr(a),"Component_Spec_List(',', (Component_Spec(Name('k'), Name('a')), Name('b')))")

        a = cls('k=a, c')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = a, c')

def test_Array_Constructor(): # R465

        cls = Array_Constructor
        a = cls('(/a/)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(/a/)')
        assert_equal(repr(a),"Array_Constructor('(/', Name('a'), '/)')")

        a = cls('[a]')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'[a]')
        assert_equal(repr(a),"Array_Constructor('[', Name('a'), ']')")

        a = cls('[integer::a]')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'[INTEGER :: a]')

        a = cls('[integer::a,b]')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'[INTEGER :: a, b]')

def test_Ac_Spec(): # R466

        cls = Ac_Spec
        a = cls('integer ::')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'INTEGER ::')
        assert_equal(repr(a),"Ac_Spec(Intrinsic_Type_Spec('INTEGER', None), None)")

        a = cls('integer :: a,b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'INTEGER :: a, b')

        a = cls('a,b')
        assert isinstance(a,Ac_Value_List),`a`
        assert_equal(str(a),'a, b')

        a = cls('integer :: a, (a, b, n = 1, 5)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'INTEGER :: a, (a, b, n = 1, 5)')

def test_Ac_Value_List(): # R469-list

        cls = Ac_Value_List
        a = cls('a, b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a, b')
        assert_equal(repr(a),"Ac_Value_List(',', (Name('a'), Name('b')))")

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

def test_Ac_Implied_Do(): # R470

        cls = Ac_Implied_Do
        a = cls('( a, b, n = 1, 5 )')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(a, b, n = 1, 5)')
        assert_equal(repr(a),"Ac_Implied_Do(Ac_Value_List(',', (Name('a'), Name('b'))), Ac_Implied_Do_Control(Name('n'), [Int_Literal_Constant('1', None), Int_Literal_Constant('5', None)]))")

def test_Ac_Implied_Do_Control(): # R471

        cls = Ac_Implied_Do_Control
        a = cls('n = 3, 5')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'n = 3, 5')
        assert_equal(repr(a),"Ac_Implied_Do_Control(Name('n'), [Int_Literal_Constant('3', None), Int_Literal_Constant('5', None)])")

        a = cls('n = 3+1, 5, 1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'n = 3 + 1, 5, 1')

###############################################################################
############################### SECTION  5 ####################################
###############################################################################

def test_Type_Declaration_Stmt(): # R501

        cls = Type_Declaration_Stmt
        a = cls('integer a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'INTEGER :: a')
        assert_equal(repr(a), "Type_Declaration_Stmt(Intrinsic_Type_Spec('INTEGER', None), None, Entity_Decl(Name('a'), None, None, None))")

        a = cls('integer ,dimension(2):: a*3')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'INTEGER, DIMENSION(2) :: a*3')

        a = cls('real a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'REAL :: a')
        assert_equal(repr(a), "Type_Declaration_Stmt(Intrinsic_Type_Spec('REAL', None), None, Entity_Decl(Name('a'), None, None, None))")

        a = cls('REAL A( LDA, * ), B( LDB, * )')
        assert isinstance(a, cls),`a`

        a = cls('DOUBLE PRECISION   ALPHA, BETA')
        assert isinstance(a, cls),`a`

def test_Declaration_Type_Spec(): # R502

        cls = Declaration_Type_Spec
        a = cls('Integer*2')
        assert isinstance(a, Intrinsic_Type_Spec),`a`
        assert_equal(str(a), 'INTEGER*2')

        a = cls('type(foo)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'TYPE(foo)')
        assert_equal(repr(a), "Declaration_Type_Spec('TYPE', Type_Name('foo'))")

def test_Attr_Spec(): # R503

        cls = Attr_Spec
        a = cls('allocatable')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'ALLOCATABLE')

        a = cls('dimension(a)')
        assert isinstance(a, Dimension_Attr_Spec),`a`
        assert_equal(str(a),'DIMENSION(a)')

def test_Dimension_Attr_Spec(): # R503.d

        cls = Dimension_Attr_Spec
        a = cls('dimension(a)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'DIMENSION(a)')
        assert_equal(repr(a),"Dimension_Attr_Spec('DIMENSION', Explicit_Shape_Spec(None, Name('a')))")

def test_Intent_Attr_Spec(): # R503.f

        cls = Intent_Attr_Spec
        a = cls('intent(in)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'INTENT(IN)')
        assert_equal(repr(a),"Intent_Attr_Spec('INTENT', Intent_Spec('IN'))")

def test_Entity_Decl(): # 504

        cls = Entity_Decl
        a = cls('a(1)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(1)')
        assert_equal(repr(a),"Entity_Decl(Name('a'), Explicit_Shape_Spec(None, Int_Literal_Constant('1', None)), None, None)")

        a = cls('a(1)*(3)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(1)*(3)')

        a = cls('a(1)*(3) = 2')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(1)*(3) = 2')

def test_Access_Spec(): # R508

        cls = Access_Spec
        a = cls('private')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PRIVATE')
        assert_equal(repr(a),"Access_Spec('PRIVATE')")

        a = cls('public')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PUBLIC')

def test_Language_Binding_Spec(): # R509

        cls = Language_Binding_Spec
        a = cls('bind(c)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'BIND(C)')
        assert_equal(repr(a),'Language_Binding_Spec(None)')

        a = cls('bind(c, name="hey")')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'BIND(C, NAME = "hey")')

def test_Explicit_Shape_Spec(): # R511

        cls = Explicit_Shape_Spec
        a = cls('a:b')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a : b')
        assert_equal(repr(a),"Explicit_Shape_Spec(Name('a'), Name('b'))")

        a = cls('a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a')

def test_Upper_Bound(): # R513

        cls = Upper_Bound
        a = cls('a')
        assert isinstance(a, Name),`a`
        assert_equal(str(a),'a')

        assertRaises(NoMatchError,cls,'*')

def test_Assumed_Shape_Spec(): # R514

        cls = Assumed_Shape_Spec
        a = cls(':')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),':')
        assert_equal(repr(a),'Assumed_Shape_Spec(None, None)')

        a = cls('a :')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a :')

def test_Deferred_Shape_Spec(): # R515

        cls = Deferred_Shape_Spec
        a = cls(':')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),':')
        assert_equal(repr(a),'Deferred_Shape_Spec(None, None)')


def test_Assumed_Size_Spec(): # R516

        cls = Assumed_Size_Spec
        a = cls('*')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'*')
        assert_equal(repr(a),'Assumed_Size_Spec(None, None)')

        a = cls('1:*')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'1 : *')

        a = cls('a,1:*')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a, 1 : *')

        a = cls('a:b,1:*')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a : b, 1 : *')

def test_Access_Stmt(): # R518

        cls = Access_Stmt
        a = cls('private')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PRIVATE')
        assert_equal(repr(a),"Access_Stmt('PRIVATE', None)")

        a = cls('public a,b')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PUBLIC :: a, b')

        a = cls('public ::a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PUBLIC :: a')

def test_Data_Stmt(): #R524
    cls = Data_Stmt
    a = cls('DATA YOURNAME % AGE, YOURNAME % NAME / 35, "FRED BROWN" /')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'DATA YOURNAME % AGE, YOURNAME % NAME / 35, "FRED BROWN" /')

    a = cls('DATA NAME / "JOHN DOE" / MILES / 10 * 0 /')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'DATA NAME / "JOHN DOE" /, MILES / 10 * 0 /')

    a = cls('DATA MYNAME / PERSON (21, \'JOHN SMITH\') /')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'DATA MYNAME / PERSON(21, \'JOHN SMITH\') /')

def test_Data_Stmt_Set(): #R525
    cls = Data_Stmt_Set
    a = cls('MILES / 10 * "2/3" /')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'MILES / 10 * "2/3" /')
        
def test_Data_Implied_Do(): # R527
    cls = Data_Implied_Do
    a = cls('((SKEW (K, J), J = 1, K), K = 1, 100)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'((SKEW(K, J), J = 1, K), K = 1, 100)')

def test_Parameter_Stmt(): # R538

        cls = Parameter_Stmt
        a = cls('parameter(a=1)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PARAMETER(a = 1)')
        assert_equal(repr(a),"Parameter_Stmt('PARAMETER', Named_Constant_Def(Name('a'), Int_Literal_Constant('1', None)))")

        a = cls('parameter(a=1, b=a+2)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PARAMETER(a = 1, b = a + 2)')

        a = cls('PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PARAMETER(ONE = 1.0D+0, ZERO = 0.0D+0)')

def test_Named_Constant_Def(): # R539

        cls = Named_Constant_Def
        a = cls('a=1')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a = 1')
        assert_equal(repr(a),"Named_Constant_Def(Name('a'), Int_Literal_Constant('1', None))")

def test_Pointer_Decl(): # R541

        cls = Pointer_Decl
        a = cls('a(:)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(:)')
        assert_equal(repr(a),"Pointer_Decl(Name('a'), Deferred_Shape_Spec(None, None))")

        a = cls('a(:,:)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(:, :)')

def test_Implicit_Stmt(): # R549

        cls = Implicit_Stmt
        a = cls('implicitnone')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'IMPLICIT NONE')
        assert_equal(repr(a),"Implicit_Stmt('IMPLICIT NONE', None)")

        a = cls('implicit real(a-d), double precision(r-t,x), type(a) (y-z)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'IMPLICIT REAL(A - D), DOUBLE PRECISION(R - T, X), TYPE(a)(Y - Z)')

def test_Implicit_Spec(): # R550

        cls = Implicit_Spec
        a = cls('integer (a-z)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'INTEGER(A - Z)')
        assert_equal(repr(a),"Implicit_Spec(Intrinsic_Type_Spec('INTEGER', None), Letter_Spec('A', 'Z'))")

        a = cls('double  complex (r,d-g)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'DOUBLE COMPLEX(R, D - G)')

def test_Letter_Spec(): # R551

        cls = Letter_Spec
        a = cls('a-z')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'A - Z')
        assert_equal(repr(a),"Letter_Spec('A', 'Z')")

        a = cls('d')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'D')

def test_Equivalence_Stmt(): # R554

        cls = Equivalence_Stmt
        a = cls('equivalence (a, b ,z)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'EQUIVALENCE(a, b, z)')
        assert_equal(repr(a),"Equivalence_Stmt('EQUIVALENCE', Equivalence_Set(Name('a'), Equivalence_Object_List(',', (Name('b'), Name('z')))))")

        a = cls('equivalence (a, b ,z),(b,l)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'EQUIVALENCE(a, b, z), (b, l)')

def test_Common_Stmt(): # R557

        cls = Common_Stmt
        a = cls('common a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'COMMON // a')
        assert_equal(repr(a),"Common_Stmt([(None, Name('a'))])")

        a = cls('common // a,b')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'COMMON // a, b')

        a = cls('common /name/ a,b')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'COMMON /name/ a, b')

        a = cls('common /name/ a,b(4,5) // c, /ljuks/ g(2)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'COMMON /name/ a, b(4, 5) // c /ljuks/ g(2)')

def test_Common_Block_Object(): # R558

        cls = Common_Block_Object
        a = cls('a(2)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(2)')
        assert_equal(repr(a),"Common_Block_Object(Name('a'), Explicit_Shape_Spec(None, Int_Literal_Constant('2', None)))")

        a = cls('a')
        assert isinstance(a, Name),`a`
        assert_equal(str(a),'a')


###############################################################################
############################### SECTION  6 ####################################
###############################################################################

def test_Substring(): # R609

        cls = Substring
        a = cls('a(:)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(:)')
        assert_equal(repr(a),"Substring(Name('a'), Substring_Range(None, None))")

        a = cls('a(1:2)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a(1 : 2)')
        assert_equal(repr(a),"Substring(Name('a'), Substring_Range(Int_Literal_Constant('1', None), Int_Literal_Constant('2', None)))")


def test_Substring_Range(): # R611

        cls = Substring_Range
        a = cls(':')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),':')
        assert_equal(repr(a),"Substring_Range(None, None)")

        a = cls('a+1:')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a + 1 :')

        a = cls('a+1: c/foo(g)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a + 1 : c / foo(g)')

        a = cls('a:b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a : b')
        assert_equal(repr(a),"Substring_Range(Name('a'), Name('b'))")

        a = cls('a:')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a :')

        a = cls(':b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),': b')


def test_Data_Ref(): # R612

        cls = Data_Ref
        a = cls('a%b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a % b')
        assert_equal(repr(a),"Data_Ref('%', (Name('a'), Name('b')))")

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

def test_Part_Ref(): # R613

        cls = Part_Ref
        a = cls('a')
        assert isinstance(a, Name),`a`
        assert_equal(str(a),'a')

def test_Type_Param_Inquiry(): # R615

        cls = Type_Param_Inquiry
        a = cls('a % b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a % b')
        assert_equal(repr(a),"Type_Param_Inquiry(Name('a'), '%', Name('b'))")


def test_Array_Section(): # R617

        cls = Array_Section
        a = cls('a(:)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a(:)')
        assert_equal(repr(a),"Array_Section(Name('a'), Substring_Range(None, None))")

        a = cls('a(2:)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a(2 :)')


def test_Section_Subscript(): # R619

        cls = Section_Subscript

        a = cls('1:2')
        assert isinstance(a, Subscript_Triplet),`a`
        assert_equal(str(a),'1 : 2')

        a = cls('zzz')
        assert isinstance(a, Name),`a`
        assert_equal(str(a),'zzz')

def test_Section_Subscript_List(): # R619-list

        cls = Section_Subscript_List
        a = cls('a,2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a, 2')
        assert_equal(repr(a),"Section_Subscript_List(',', (Name('a'), Int_Literal_Constant('2', None)))")

        a = cls('::1')
        assert isinstance(a,Subscript_Triplet),`a`
        assert_equal(str(a),': : 1')

        a = cls('::1, 3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),': : 1, 3')

def test_Subscript_Triplet(): # R620

        cls = Subscript_Triplet
        a = cls('a:b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a : b')
        assert_equal(repr(a),"Subscript_Triplet(Name('a'), Name('b'), None)")

        a = cls('a:b:1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a : b : 1')

        a = cls(':')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),':')

        a = cls('::5')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),': : 5')

        a = cls(':5')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),': 5')

        a = cls('a+1 :')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a + 1 :')

def test_Alloc_Opt(): # R624

        cls = Alloc_Opt
        a = cls('stat=a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'STAT = a')
        assert_equal(repr(a),"Alloc_Opt('STAT', Name('a'))")

def test_Nullify_Stmt(): # R633

        cls = Nullify_Stmt
        a = cls('nullify (a)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'NULLIFY(a)')
        assert_equal(repr(a),"Nullify_Stmt('NULLIFY', Name('a'))")

        a = cls('nullify (a,c)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'NULLIFY(a, c)')

###############################################################################
############################### SECTION  7 ####################################
###############################################################################

def test_Primary(): # R701

        cls = Primary
        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

        a = cls('(a)')
        assert isinstance(a,Parenthesis),`a`
        assert_equal(str(a),'(a)')

        a = cls('1')
        assert isinstance(a,Int_Literal_Constant),`a`
        assert_equal(str(a),'1')

        a = cls('1.')
        assert isinstance(a,Real_Literal_Constant),`a`
        assert_equal(str(a),'1.')

        a = cls('(1, n)')
        assert isinstance(a,Complex_Literal_Constant),`a`
        assert_equal(str(a),'(1, n)')

        a = cls('.true.')
        assert isinstance(a,Logical_Literal_Constant),`a`
        assert_equal(str(a),'.TRUE.')

        a = cls('"hey a()c"')
        assert isinstance(a,Char_Literal_Constant),`a`
        assert_equal(str(a),'"hey a()c"')

        a = cls('b"0101"')
        assert isinstance(a,Binary_Constant),`a`
        assert_equal(str(a),'B"0101"')

        a = cls('o"0107"')
        assert isinstance(a,Octal_Constant),`a`
        assert_equal(str(a),'O"0107"')

        a = cls('z"a107"')
        assert isinstance(a,Hex_Constant),`a`
        assert_equal(str(a),'Z"A107"')

        a = cls('a % b')
        assert isinstance(a,Data_Ref),`a`
        assert_equal(str(a),'a % b')

        a = cls('a(:)')
        assert isinstance(a,Array_Section),`a`
        assert_equal(str(a),'a(:)')

        a = cls('0.0E-1')
        assert isinstance(a,Real_Literal_Constant),`a`
        assert_equal(str(a),'0.0E-1')

def test_Parenthesis(): # R701.h

        cls = Parenthesis
        a  = cls('(a)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(a)')
        assert_equal(repr(a),"Parenthesis('(', Name('a'), ')')")

        a  = cls('(a+1)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(a + 1)')

        a  = cls('((a))')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'((a))')

        a  = cls('(a+(a+c))')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(a + (a + c))')

def test_Level_1_Expr(): # R702

        cls = Level_1_Expr
        a = cls('.hey. a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.HEY. a')
        assert_equal(repr(a),"Level_1_Expr('.HEY.', Name('a'))")

        #assertRaises(NoMatchError,cls,'.not. a')

        a = cls('.false.')
        assert isinstance(a,Logical_Literal_Constant),`a`

def test_Mult_Operand(): # R704

        cls = Mult_Operand
        a = cls('a**b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a ** b')
        assert_equal(repr(a),"Mult_Operand(Name('a'), '**', Name('b'))")

        a = cls('a**2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a ** 2')

        a = cls('(a+b)**2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(a + b) ** 2')

        a = cls('0.0E-1')
        assert isinstance(a,Real_Literal_Constant),`a`
        assert_equal(str(a),'0.0E-1')

def test_Add_Operand(): # R705

        cls = Add_Operand
        a = cls('a*b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a * b')
        assert_equal(repr(a),"Add_Operand(Name('a'), '*', Name('b'))")

        a = cls('a/b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a / b')

        a = cls('a**b')
        assert isinstance(a,Mult_Operand),`a`
        assert_equal(str(a),'a ** b')

        a = cls('0.0E-1')
        assert isinstance(a,Real_Literal_Constant),`a`
        assert_equal(str(a),'0.0E-1')

def test_Level_2_Expr(): # R706

        cls = Level_2_Expr
        a = cls('a+b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a + b')
        assert_equal(repr(a),"Level_2_Expr(Name('a'), '+', Name('b'))")

        a = cls('a-b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a - b')

        a = cls('a+b+c')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a + b + c')

        a = cls('+a')
        assert isinstance(a,Level_2_Unary_Expr),`a`
        assert_equal(str(a),'+ a')

        a = cls('+1')
        assert isinstance(a,Level_2_Unary_Expr),`a`
        assert_equal(str(a),'+ 1')

        a = cls('+a+b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+ a + b')

        a = cls('0.0E-1')
        assert isinstance(a,Real_Literal_Constant),`a`
        assert_equal(str(a),'0.0E-1')


def test_Level_2_Unary_Expr():

        cls = Level_2_Unary_Expr
        a = cls('+a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+ a')
        assert_equal(repr(a),"Level_2_Unary_Expr('+', Name('a'))")

        a = cls('-a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'- a')

        a = cls('+1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+ 1')

        a = cls('0.0E-1')
        assert isinstance(a,Real_Literal_Constant),`a`
        assert_equal(str(a),'0.0E-1')


def test_Level_3_Expr(): # R710

        cls = Level_3_Expr
        a = cls('a//b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a // b')
        assert_equal(repr(a),"Level_3_Expr(Name('a'), '//', Name('b'))")

        a = cls('"a"//"b"')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'"a" // "b"')

def test_Level_4_Expr(): # R712

        cls = Level_4_Expr
        a = cls('a.eq.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .EQ. b')
        assert_equal(repr(a),"Level_4_Expr(Name('a'), '.EQ.', Name('b'))")

        a = cls('a.ne.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .NE. b')

        a = cls('a.lt.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .LT. b')

        a = cls('a.gt.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .GT. b')

        a = cls('a.ge.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .GE. b')

        a = cls('a==b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a == b')

        a = cls('a/=b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a /= b')

        a = cls('a<b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a < b')

        a = cls('a<=b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a <= b')

        a = cls('a>=b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a >= b')

        a = cls('a>b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a > b')

def test_And_Operand(): # R714

        cls = And_Operand
        a = cls('.not.a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.NOT. a')
        assert_equal(repr(a),"And_Operand('.NOT.', Name('a'))")

def test_Or_Operand(): # R715

        cls = Or_Operand
        a = cls('a.and.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .AND. b')
        assert_equal(repr(a),"Or_Operand(Name('a'), '.AND.', Name('b'))")


def test_Equiv_Operand(): # R716

        cls = Equiv_Operand
        a = cls('a.or.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .OR. b')
        assert_equal(repr(a),"Equiv_Operand(Name('a'), '.OR.', Name('b'))")


def test_Level_5_Expr(): # R717

        cls = Level_5_Expr
        a = cls('a.eqv.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .EQV. b')
        assert_equal(repr(a),"Level_5_Expr(Name('a'), '.EQV.', Name('b'))")

        a = cls('a.neqv.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .NEQV. b')

        a = cls('a.eq.b')
        assert isinstance(a,Level_4_Expr),`a`
        assert_equal(str(a),'a .EQ. b')

def test_Expr(): # R722

        cls = Expr
        a = cls('a .op. b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .OP. b')
        assert_equal(repr(a),"Expr(Name('a'), '.OP.', Name('b'))")

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

        a = cls('3.e2')
        assert isinstance(a,Real_Literal_Constant),`a`

        a = cls('0.0E-1')
        assert isinstance(a,Real_Literal_Constant),`a`
        assert_equal(str(a),'0.0E-1')

        a = cls('123')
        assert isinstance(a,Int_Literal_Constant),`a`
        assert_equal(str(a),'123')

        a = cls('.false.')
        assert isinstance(a,Logical_Literal_Constant),`a`
        assert_equal(str(a),'.FALSE.')

        assertRaises(NoMatchError,Scalar_Int_Expr,'a,b')

def test_Logical_Expr(): # R733

    cls = Logical_Expr
    a = cls('.false.')
    assert isinstance(a, Logical_Literal_Constant), `a`
    assert str(a)=='.FALSE.'

def test_Assignment_Stmt(): # R734

        cls = Assignment_Stmt
        a = cls('a = b')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a = b')
        assert_equal(repr(a),"Assignment_Stmt(Name('a'), '=', Name('b'))")

        a = cls('a(3:4) = b+c')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(3 : 4) = b + c')

        a = cls('a%c = b+c')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a % c = b + c')

        a = cls('a = .FALSE.')
        assert isinstance(a, cls),`a`
        assert_equal(repr(a),"Assignment_Stmt(Name('a'), '=', Logical_Literal_Constant('.FALSE.', None))")

def test_Proc_Component_Ref(): # R741

        cls = Proc_Component_Ref
        a = cls('a % b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a % b')
        assert_equal(repr(a),"Proc_Component_Ref(Name('a'), '%', Name('b'))")

def test_Where_Stmt(): # R743

        cls = Where_Stmt
        a = cls('where (a) c=2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'WHERE (a) c = 2')
        assert_equal(repr(a),"Where_Stmt(Name('a'), Assignment_Stmt(Name('c'), '=', Int_Literal_Constant('2', None)))")

def test_Where_Construct_Stmt(): # R745

        cls = Where_Construct_Stmt
        a = cls('where (a)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'WHERE (a)')
        assert_equal(repr(a),"Where_Construct_Stmt(Name('a'))")


###############################################################################
############################### SECTION  8 ####################################
###############################################################################

def test_Label_Do_Stmt(): # R828

    cls = Label_Do_Stmt
    a = cls('do 12')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'DO 12')
    assert_equal(repr(a),"Label_Do_Stmt(None, Label('12'), None)")

def test_Continue_Stmt(): # R848

        cls = Continue_Stmt
        a = cls('continue')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'CONTINUE')
        assert_equal(repr(a),"Continue_Stmt('CONTINUE')")

###############################################################################
############################### SECTION  9 ####################################
###############################################################################

def test_Io_Unit(): # R901

        cls = Io_Unit
        a = cls('*')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'*')

        a = cls('a')
        assert isinstance(a, Name),`a`
        assert_equal(str(a),'a')

def test_Read_Stmt(): # R910
    cls = Read_Stmt
    a = cls('read(123)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a), 'READ(UNIT = 123)')

    a = cls('read(123) a')
    assert_equal(str(a), 'READ(UNIT = 123) a')
    a = cls('read(123) a(  2)')
    assert_equal(str(a), 'READ(UNIT = 123) a(2)')

    a = cls('read*, a(  2), b')
    assert_equal(str(a), 'READ *, a(2), b')
    
def test_Write_Stmt(): # R911

        cls = Write_Stmt
        a = cls('write (123)"hey"')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'WRITE(UNIT = 123) "hey"')
        assert_equal(repr(a),'Write_Stmt(Io_Control_Spec_List(\',\', (Io_Control_Spec(\'UNIT\', Int_Literal_Constant(\'123\', None)),)), Char_Literal_Constant(\'"hey"\', None))')

def test_Print_Stmt(): # R912

        cls = Print_Stmt
        a = cls('print 123')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PRINT 123')
        assert_equal(repr(a),"Print_Stmt(Label('123'), None)")

        a = cls('print *,"a=",a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PRINT *, "a=", a')

def test_Io_Control_Spec(): # R913

        cls = Io_Control_Spec
        a = cls('end=123')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'END = 123')
        assert_equal(repr(a),"Io_Control_Spec('END', Label('123'))")

def test_Io_Control_Spec_List(): # R913-list

        cls = Io_Control_Spec_List
        a = cls('end=123')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'END = 123')
        assert_equal(repr(a),"Io_Control_Spec_List(',', (Io_Control_Spec('END', Label('123')),))")

        a = cls('123')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'UNIT = 123')

        a = cls('123,*')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'UNIT = 123, FMT = *')

        a = cls('123,fmt=a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'UNIT = 123, FMT = a')

        if 0:
            # see todo note in Io_Control_Spec_List
            a = cls('123,a')
            assert isinstance(a, cls),`a`
            assert_equal(str(a),'UNIT = 123, NML = a')

def test_Format(): # R914

        cls = Format
        a = cls('*')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'*')
        assert_equal(repr(a),"Format('*')")

        a = cls('a')
        assert isinstance(a, Name),`a`
        assert_equal(str(a),'a')

        a = cls('123')
        assert isinstance(a, Label),`a`
        assert_equal(str(a),'123')

def test_Wait_Stmt(): # R921

        cls = Wait_Stmt
        a = cls('wait (123)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'WAIT(UNIT = 123)')

def test_Wait_Spec(): # R922

        cls = Wait_Spec
        a = cls('123')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'UNIT = 123')
        assert_equal(repr(a),"Wait_Spec('UNIT', Int_Literal_Constant('123', None))")

        a = cls('err=1')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'ERR = 1')

def test_Backspace_Stmt(): # R923

    cls = Backspace_Stmt
    a = cls('backspace 1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'BACKSPACE 1')

    a = cls('backspace  (unit=1,err=2)')
    assert_equal(str(a),'BACKSPACE(UNIT = 1, ERR = 2)')

def test_Endfile_Stmt(): # R924

    cls = Endfile_Stmt
    a = cls('endfile 1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'ENDFILE 1')

    a = cls('endfile  (unit=1,err=2)')
    assert_equal(str(a),'ENDFILE(UNIT = 1, ERR = 2)')

def test_Rewind_Stmt(): # R925

    cls = Rewind_Stmt
    a = cls('rewind 1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'REWIND 1')

    a = cls('rewind  (unit=1,err=2)')
    assert_equal(str(a),'REWIND(UNIT = 1, ERR = 2)')

def test_Position_Spec(): # R926

    cls = Position_Spec
    a = cls('1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'UNIT = 1')
    a = cls('unit=1')
    assert_equal(str(a),'UNIT = 1')
    a = cls('err=2')
    assert_equal(str(a),'ERR = 2')
    a = cls('iomsg=a')
    assert_equal(str(a),'IOMSG = a')
    a = cls('iostat=a')
    assert_equal(str(a),'IOSTAT = a')

def test_Flush_Stmt(): # R927

    cls = Flush_Stmt
    a = cls('flush 1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'FLUSH 1')

    a = cls('flush  (unit=1,err=2)')
    assert_equal(str(a),'FLUSH(UNIT = 1, ERR = 2)')

def test_Flush_Spec(): # R928

    cls = Flush_Spec
    a = cls('1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'UNIT = 1')
    a = cls('unit=1')
    assert_equal(str(a),'UNIT = 1')
    a = cls('err=2')
    assert_equal(str(a),'ERR = 2')
    a = cls('iomsg=a')
    assert_equal(str(a),'IOMSG = a')
    a = cls('iostat=a')
    assert_equal(str(a),'IOSTAT = a')

def test_Inquire_Stmt(): # R929

    cls = Inquire_Stmt
    a = cls('inquire(1,file=a)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'INQUIRE(UNIT = 1, FILE = a)')
    a = cls('inquire(iolength=n) a, b')
    assert_equal(str(a),'INQUIRE(IOLENGTH=n) a, b')

def test_Inquire_Spec(): # R930

    cls = Inquire_Spec
    a = cls('1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'UNIT = 1')
    a = cls('file=fn')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'FILE = fn')

    a = cls('access=a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'ACCESS = a')


    
###############################################################################
############################### SECTION 10 ####################################
###############################################################################


###############################################################################
############################### SECTION 11 ####################################
###############################################################################

def test_Use_Stmt(): # R1109

        cls = Use_Stmt
        a = cls('use a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'USE :: a')
        assert_equal(repr(a),"Use_Stmt(None, Name('a'), '', None)")

        a = cls('use :: a, c=>d')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'USE :: a, c => d')

        a = cls('use :: a, operator(.hey.)=>operator(.hoo.)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'USE :: a, OPERATOR(.HEY.) => OPERATOR(.HOO.)')

        a = cls('use, intrinsic :: a, operator(.hey.)=>operator(.hoo.), c=>g')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'USE, INTRINSIC :: a, OPERATOR(.HEY.) => OPERATOR(.HOO.), c => g')

def test_Module_Nature(): # R1110

        cls = Module_Nature
        a = cls('intrinsic')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'INTRINSIC')
        assert_equal(repr(a),"Module_Nature('INTRINSIC')")

        a = cls('non_intrinsic')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'NON_INTRINSIC')

###############################################################################
############################### SECTION 12 ####################################
###############################################################################

def test_Proc_Attr_Spec(): # R1213
    cls = Proc_Attr_Spec
    a = cls('intent(in)')
    assert isinstance(a, cls)
    assert_equal(str(a),'INTENT(IN)')

    a = cls('optional')
    assert isinstance(a, cls)
    assert_equal(str(a),'OPTIONAL')

    a = cls('save')
    assert isinstance(a, cls)
    assert_equal(str(a),'SAVE')

    a = cls('private')
    assert isinstance(a, Access_Spec),`type(a)`
    assert_equal(str(a),'PRIVATE')

    a = cls('bind(c)')
    assert isinstance(a, Language_Binding_Spec),`a`
    assert_equal(str(a),'BIND(C)')

def test_Function_Reference(): # R1217

        cls = Function_Reference
        a = cls('f()')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'f()')
        assert_equal(repr(a),"Function_Reference(Name('f'), None)")

        a = cls('f(2,k=1,a)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'f(2, k = 1, a)')


def test_Procedure_Designator(): # R1219

        cls = Procedure_Designator
        a = cls('a%b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a % b')
        assert_equal(repr(a),"Procedure_Designator(Name('a'), '%', Name('b'))")

def test_Actual_Arg_Spec(): # R1220

        cls = Actual_Arg_Spec
        a = cls('k=a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = a')
        assert_equal(repr(a),"Actual_Arg_Spec(Name('k'), Name('a'))")

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

def test_Actual_Arg_Spec_List():

        cls = Actual_Arg_Spec_List
        a = cls('a,b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a, b')
        assert_equal(repr(a),"Actual_Arg_Spec_List(',', (Name('a'), Name('b')))")

        a = cls('a = k')
        assert isinstance(a,Actual_Arg_Spec),`a`
        assert_equal(str(a),'a = k')

        a = cls('a = k,b')
        assert isinstance(a,Actual_Arg_Spec_List),`a`
        assert_equal(str(a),'a = k, b')

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

def test_Alt_Return_Spec(): # R1222

        cls = Alt_Return_Spec
        a = cls('* 123')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'*123')
        assert_equal(repr(a),"Alt_Return_Spec(Label('123'))")

def test_Function_Subprogram(): # R1223

    reader = get_reader('''\
    function foo()
    end function foo''')
    cls = Function_Subprogram
    a = cls(reader)
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'FUNCTION foo()\nEND FUNCTION foo')
    assert_equal(repr(a),"Function_Subprogram(Function_Stmt(None, Name('foo'), None, None), End_Function_Stmt('FUNCTION', Name('foo')))")

    reader = get_reader('''\
    pure real function foo(a) result(b) bind(c)
    integer a
    end function foo''')
    cls = Function_Subprogram
    a = cls(reader)
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'PURE REAL FUNCTION foo(a) RESULT(b) BIND(C)\n  INTEGER :: a\nEND FUNCTION foo')

def test_Function_Stmt(): # R1224
        cls = Function_Stmt
        a = cls('function foo()')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'FUNCTION foo()')
        assert_equal(repr(a),"Function_Stmt(None, Name('foo'), None, None)")

        a = cls('function foo(a,b)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'FUNCTION foo(a, b)')
        assert_equal(repr(a),"Function_Stmt(None, Name('foo'), Dummy_Arg_List(',', (Name('a'), Name('b'))), None)")    

        a = cls('function foo(a)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'FUNCTION foo(a)')

        a = cls('real function foo(a)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'REAL FUNCTION foo(a)')

        a = cls('real recursive function foo(a)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'REAL RECURSIVE FUNCTION foo(a)')

        a = cls('real function foo(a) bind(c)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'REAL FUNCTION foo(a) BIND(C)')

        a = cls('real function foo(a) result (b)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'REAL FUNCTION foo(a) RESULT(b)')

        a = cls('real function foo(a) bind(c) result(b)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'REAL FUNCTION foo(a) RESULT(b) BIND(C)')

def test_Dummy_Arg_Name(): # R1226
    cls = Dummy_Arg_Name
    a = cls('a')
    assert isinstance(a, Name),`a`
    assert_equal(str(a),'a')

def test_Prefix(): # R1227

        cls = Prefix
        a = cls('pure  recursive')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PURE RECURSIVE')
        assert_equal(repr(a), "Prefix(' ', (Prefix_Spec('PURE'), Prefix_Spec('RECURSIVE')))")

        a = cls('integer * 2 pure')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'INTEGER*2 PURE')

def test_Prefix_Spec(): # R1228

        cls = Prefix_Spec
        a = cls('pure')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PURE')
        assert_equal(repr(a),"Prefix_Spec('PURE')")

        a = cls('elemental')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'ELEMENTAL')

        a = cls('recursive')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'RECURSIVE')

        a = cls('integer * 2')
        assert isinstance(a, Intrinsic_Type_Spec),`a`
        assert_equal(str(a),'INTEGER*2')

def test_Suffix(): # R1229

    cls = Suffix
    
    a = cls('bind(c)')
    assert isinstance(a, Language_Binding_Spec),`a`
    assert_equal(str(a),'BIND(C)')
    assert_equal(repr(a),"Language_Binding_Spec(None)")

    a = cls('result(a)')
    assert isinstance(a, Suffix),`a`
    assert_equal(str(a),'RESULT(a)')

    a = cls('bind(c) result(a)')
    assert isinstance(a, Suffix),`a`
    assert_equal(str(a),'RESULT(a) BIND(C)')

    a = cls('result(a) bind(c)')
    assert isinstance(a, Suffix),`a`
    assert_equal(str(a),'RESULT(a) BIND(C)')

def test_End_Function_Stmt(): # R1230
    cls = End_Function_Stmt
    a = cls('end')
    assert isinstance(a, cls),`a`
    assert_equal(str(a), 'END FUNCTION')

    a = cls('endfunction')
    assert_equal(str(a), 'END FUNCTION')

    a = cls('endfunction foo')
    assert_equal(str(a), 'END FUNCTION foo')

def test_Subroutine_Subprogram(): # R1231

        reader = get_reader('''\
      subroutine foo
      end subroutine foo''')
        cls = Subroutine_Subprogram
        a = cls(reader)
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo\nEND SUBROUTINE foo')
        assert_equal(repr(a),"Subroutine_Subprogram(Subroutine_Stmt(None, Name('foo'), None, None), End_Subroutine_Stmt('SUBROUTINE', Name('foo')))")

        reader = get_reader('''\
      subroutine foo
        integer a
      end subroutine foo''')
        cls = Subroutine_Subprogram
        a = cls(reader)
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo\n  INTEGER :: a\nEND SUBROUTINE foo')

def test_Subroutine_Stmt(): # R1232

        cls = Subroutine_Stmt
        a = cls('subroutine foo')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo')
        assert_equal(repr(a),"Subroutine_Stmt(None, Name('foo'), None, None)")

        a = cls('pure subroutine foo')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PURE SUBROUTINE foo')

        a = cls('pure subroutine foo(a,b)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PURE SUBROUTINE foo(a, b)')

        a = cls('subroutine foo() bind(c)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo BIND(C)')

        a = cls('subroutine foo(a)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo(a)')

        a = cls('subroutine foo(a, b)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo(a, b)')

        a = cls('subroutine foo(a,*)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo(a, *)')

        a = cls('subroutine foo(*)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo(*)')

def test_Dummy_Arg(): # R1233
    cls = Dummy_Arg
    a = cls('a')
    assert isinstance(a, Name),`a`
    assert_equal(str(a),'a')
    a = cls('*')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'*')
    
def test_End_Subroutine_Stmt(): # R1234

        cls = End_Subroutine_Stmt
        a = cls('end subroutine foo')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'END SUBROUTINE foo')
        assert_equal(repr(a),"End_Subroutine_Stmt('SUBROUTINE', Name('foo'))")

        a = cls('end')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'END SUBROUTINE')

        a = cls('endsubroutine')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'END SUBROUTINE')

def test_Entry_Stmt(): # R1235

    cls = Entry_Stmt
    a = cls('entry a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a), 'ENTRY a()')

    a = cls('entry a()')
    assert_equal(str(a), 'ENTRY a()')

    a = cls('entry a(b, c)')
    assert_equal(str(a), 'ENTRY a(b, c)')

    a = cls('entry a(b, c) bind(c)')
    assert_equal(str(a), 'ENTRY a(b, c) BIND(C)')


def test_Return_Stmt(): # R1236

        cls = Return_Stmt
        a = cls('return')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'RETURN')
        assert_equal(repr(a), 'Return_Stmt(None)')

def test_Contains(): # R1237

        cls = Contains_Stmt
        a = cls('Contains')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'CONTAINS')
        assert_equal(repr(a),"Contains_Stmt('CONTAINS')")

if 1:
    nof_needed_tests = 0
    nof_needed_match = 0
    total_needs = 0
    total_classes = 0
    for name in dir():
        obj = eval(name)
        if not isinstance(obj, ClassType): continue
        if not issubclass(obj, Base): continue
        clsname = obj.__name__
        if clsname.endswith('Base'): continue
        total_classes += 1
        subclass_names = obj.__dict__.get('subclass_names',None)
        use_names = obj.__dict__.get('use_names',None)
        if not use_names: continue
        match = obj.__dict__.get('match',None)
        try:
            test_cls = eval('test_%s' % (clsname))
        except NameError:
            test_cls = None
        total_needs += 1
        if match is None:
            if test_cls is None:
                #print 'Needs tests:', clsname
                print 'Needs match implementation:', clsname
                nof_needed_tests += 1
                nof_needed_match += 1
            else:
                print 'Needs match implementation:', clsname
                nof_needed_match += 1
        else:
            if test_cls is None:
                #print 'Needs tests:', clsname
                nof_needed_tests += 1
        continue
    print '-----'
    print 'Nof match implementation needs:',nof_needed_match,'out of',total_needs
    print 'Nof tests needs:',nof_needed_tests,'out of',total_needs
    print 'Total number of classes:',total_classes
    print '-----'
