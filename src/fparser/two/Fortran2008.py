'''The file implements the Fortran2008 rules as defined in
    https://j3-fortran.org/doc/year/10/10-007r1.pdf

'''
from fparser.two.Fortran2003 import Program_Unit as Program_Unit_2003
from fparser.two.Fortran2003 import EndStmtBase
from fparser.two.Fortran2003 import BlockBase
from fparser.two.Fortran2003 import Base
from fparser.common.splitline import splitparen
from fparser.two.Fortran2003 import Specification_Part, Module_Subprogram_Part
# pylint: disable=invalid-name
# pylint: disable=arguments-differ
# pylint: disable=undefined-variable
# pylint: disable=eval-used
# pylint: disable=exec-used


class Program_Unit(Program_Unit_2003):  # R202
    '''
    Fortran 2008 rule R202
    program-unit is main-program
                    or external-subprogram
                    or module
                    or submodule
                    or block-data

    '''

    # Fortran2008 adds the concept of submodules to a program-unit. We
    # therefore extend the Fortran2003 specification
    subclass_names = Program_Unit_2003.subclass_names[:]
    subclass_names.append("Sub_Module")


class Sub_Module(BlockBase):  # R1116
    '''
    Fortran 2008 rule R1116
    submodule is submodule-stmt
                 [ specification-part ]
                 [ module-subprogram-part ]
                 end-submodule-stmt

    '''

    subclass_names = []
    use_names = ['Sub_Module_Stmt', 'Specification_Part',
                 'Module_Subprogram_Part', 'End_Sub_Module_Stmt']

    @staticmethod
    def match(reader):
        '''Check whether the input matches the rule

        param reader: the fortran file reader containing the line(s)
                      of code that we are trying to match
        :type reader: :py:class:`fparser.common.readfortran.FortranFileReader`
                      or
                      :py:class:`fparser.common.readfortran.FortranStringReader`
        :return: `tuple` containing a single `list` which contains
                 instance of the classes that have matched if there is
                 a match or `None` if there is no match

        '''

        result = BlockBase.match(Sub_Module_Stmt,
                                 [Specification_Part, Module_Subprogram_Part],
                                 End_Sub_Module_Stmt, reader)
        return result


class Sub_Module_Stmt(Base):  # R1117
    '''
    Fortran 2008 rule R1117
    submodule-stmt is SUBMODULE ( parent-identifier ) submodule-name

    '''
    subclass_names = []
    use_names = ['Sub_Module_Name', 'Parent_Identifier']

    @staticmethod
    def match(fstring):
        '''Check whether the input matches the rule

        param string fstring : contains the Fortran that we are trying
        to match

        :return: instances of the Classes that have matched if there
        is a match or `None` if there is no match

        '''
        # First look for "SUBMODULE"
        name = "SUBMODULE"
        if fstring[:len(name)].upper() != name:
            # the string does not start with SUBMODULE so does not
            # match
            return None
        # "SUBMODULE is found so strip it out and split the remaining
        # line by parenthesis
        splitline = splitparen(fstring[len(name):].lstrip())
        # We expect 2 entries, the first being parent_identifier with
        # brackets and the second submodule_name. However for some
        # reason we get an additional empty 1st entry when using
        # splitline
        if len(splitline) != 3:
            # format is not ( parent_identifier ) submodule_name
            return None
        spurious = splitline[0]
        parent_id_brackets = splitline[1]
        submodule_name = splitline[2]
        if spurious:
            # this should be empty
            return None
        # Make sure the parent identifier contained in splitline[1] is
        # enclosed with round brackets
        if parent_id_brackets[0] != "(":
            return None
        if parent_id_brackets[-1] != ")":
            return None
        parent_id = parent_id_brackets[1:-1].lstrip().rstrip()
        # Format is OK from this Class' perspective. Pass on
        # parent_identifier and submodule name to appropriate classes
        return Parent_Identifier(parent_id), Sub_Module_Name(submodule_name)

    def tostr(self):
        '''return the fortran representation of this object'''
        # return self.string  # this returns the original code
        return "SUBMODULE ({0}) {1}".format(self.items[0], self.items[1])

    def get_name(self):  # C1114
        '''Fortran 2008 constraint C1114
        return the submodule name. This is used by the base class to check
        whether the submodule name matches the name used for the end
        submodule statement if one is provided.

        :return: the name of the submodule stored in a Name class
        :return type: :py:class:`fparser.two.Fortran2003.Name`
        '''
        return self.items[1]


class End_Sub_Module_Stmt(EndStmtBase):  # R1119
    '''
    Fortran 2008 rule R1119
    end-submodule-stmt is END [ SUBMODULE [ submodule-name ] ]

    '''
    subclass_names = []
    use_names = ['Sub_Module_Name']

    @staticmethod
    def match(fstring):
        '''Check whether the input matches the rule

        param string fstring : contains the Fortran that we are trying
        to match

        :return: instances of the Classes that have matched if there
        is a match or `None` if there is no match

        '''
        return EndStmtBase.match('SUBMODULE', Sub_Module_Name, fstring)

    def get_name(self):  # C1114
        '''Fortran 2008 constraint C1114 return the submodule name as
        specified by the end submodule statement or `None` if one is
        not specified. This is used by the base class to check whether
        this name matches the submodule name.

        :return: the name of the submodule stored in a Name class
        :return type: :py:class:`fparser.two.Fortran2003.Name` or `None`

        '''
        return self.items[1]


class Parent_Identifier(Base):  # R1118
    '''
    Fortran 2008 rule R1118
    parent-identifier is ancestor-module-name [ : parent-submodule-name ]

    '''

    use_names = ['Ancestor_Module_Name', 'Parent_SubModule_Name']

    @staticmethod
    def match(fstring):
        '''Check whether the input matches the rule

        param string fstring : contains the Fortran that we are trying
        to match

        :return: instances of the Classes that have matched if there
        is a match or `None` if there is no match

        '''
        split_string = fstring.split(":")
        len_split_string = len(split_string)
        lhs_name = split_string[0].lstrip().rstrip()
        if len_split_string == 1:
            return Ancestor_Module_Name(lhs_name), None
        elif len_split_string == 2:
            rhs_name = split_string[1].lstrip().rstrip()
            return Ancestor_Module_Name(lhs_name), \
                Parent_SubModule_Name(rhs_name)
        # we expect at most one ':' in our input so the match fails
        return None, None

    def tostr(self):
        '''return the fortran representation of this object'''
        # return self.string  # this returns the original code
        if self.items[1]:
            return "{0}:{1}".format(self.items[0], self.items[1])
        else:
            return str(self.items[0])

#
# GENERATE Scalar_, _List, _Name CLASSES
#


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
