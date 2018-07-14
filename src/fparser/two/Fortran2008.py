from fparser.two.Fortran2003 import Program_Unit as Program_Unit_2003
from fparser.two.Fortran2003 import StmtBase, WORDClsBase, EndStmtBase
from fparser.two.Fortran2003 import BlockBase


class Program_Unit(Program_Unit_2003):  # R202
    """
:F08R:`202`::
    <program-unit> = <main-program>
                     | <external-subprogram>
                     | <module>
                     | <block-data>
                     | <submodule>
    """

    
    subclass_names = Program_Unit_2003.subclass_names[:]
    subclass_names.append("Sub_Module")


class Sub_Module(BlockBase): # Rxxxx
    ''' xxx '''
    subclass_names = []
    use_names = ['Sub_Module_Stmt', 'End_Sub_Module_Stmt']

    @staticmethod
    def match(reader):
        return BlockBase.match(Sub_Module_Stmt,
                               [],
                               End_Sub_Module_Stmt, reader)


class Sub_Module_Stmt(StmtBase, WORDClsBase):  # Rxxxx
    
    subclass_names = []
    use_names = ['Sub_Module_Name']

    @staticmethod
    def match(string):
        return WORDClsBase.match('SUBMODULE', Sub_Module_Name, string,
                                 require_cls=True)


class End_Sub_Module_Stmt(EndStmtBase):  # Rxxxx
    """
    <end-submodule-stmt> = END [ SUBMODULE [ <module-name> ] ]
    """
    subclass_names = []
    use_names = ['Sub_Module_Name']

    @staticmethod
    def match(string):
        return EndStmtBase.match('SUBMODULE', Sub_Module_Name, string)

    def get_name(self):
        return self.items[1]

#
# GENERATE Scalar_, _List, _Name CLASSES
#

from fparser.two.Fortran2003 import Base
ClassType = type(Base)
_names = dir()
for clsname in _names:
    cls = eval(clsname)
    if not (isinstance(cls, ClassType) and issubclass(cls, Base) and
            not cls.__name__.endswith('Base')):
        continue

    names = getattr(cls, 'subclass_names', []) + getattr(cls, 'use_names', [])
    for n in names:
        if n in _names:
            continue
        if n.endswith('_List'):
            _names.append(n)
            n = n[:-5]
            # Generate 'list' class
            exec('''\
class %s_List(SequenceBase):
    subclass_names = [\'%s\']
    use_names = []
    def match(string): return SequenceBase.match(r\',\', %s, string)
    match = staticmethod(match)
''' % (n, n, n))
        elif n.endswith('_Name'):
            _names.append(n)
            n = n[:-5]
            exec('''\
class %s_Name(Base):
    subclass_names = [\'Name\']
''' % (n))
        elif n.startswith('Scalar_'):
            _names.append(n)
            n = n[7:]
            exec('''\
class Scalar_%s(Base):
    subclass_names = [\'%s\']
''' % (n, n))
