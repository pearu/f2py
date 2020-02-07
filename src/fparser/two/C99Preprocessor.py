#!/usr/bin/env python

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

"""C99 Preprocessor Syntax Rules.
"""
# Author: Balthasar Reuter <balthasar.reuter@ecmwf.int>
# Based on previous work by Martin Schlipf (https://github.com/martin-schlipf)
# First version created: Jan 2020

import re

from fparser.two import pattern_tools as pattern
from fparser.two.utils import (Base, StringBase, InternalError)
from fparser.two.Fortran2003 import Include_Filename


def match_cpp_directive(reader):
    '''Create single-line C99 preprocessor directive object from the current
    line. Omit if-construct and its members that are dealt with separately.'''
    cls_list = [Cpp_If_Stmt, Cpp_Elif_Stmt, Cpp_Else_Stmt,
                Cpp_Endif_Stmt, Cpp_Include_Stmt, Cpp_Macro_Stmt,
                Cpp_Undef_Stmt, Cpp_Line_Stmt, Cpp_Error_Stmt,
                Cpp_Warning_Stmt, Cpp_Null_Stmt]
    for cls in cls_list:
        obj = cls(reader)
        if obj:
            return obj
    return None

#
# ISO/IEC 9899: 1999 (C99)
#

#
# 6.10 Preprocessing directives
#


#
# Below construct for if-elif-else-endif blocks of preprocessor macros works
# as long as they are within a common block of the Fortran code. However,
# since preprocessor macros can potentially span, e.g., specification part and
# execution part, this does not comply to the AST format of fparser. Hence,
# this is commented out and if-stmts etc are treated as one-line pp macros
# without being correlated to their counterparts.
#

# class Program(Program_2003):
#
#     # Add Preprocess if-constructs to the Program class by extending
#     # the specification
#     subclass_names = Program_2003.subclass_names[:]
#     subclass_names.append('Cpp_If_Construct')


# class Cpp_If_Construct(BlockBase):  # 6.10.1 Conditional inclusion
#     '''
#     C99 6.10.1 Conditional inclusion
#
#     <cpp-if-construct> = <cpp-if-stmt>
#                             [ <block> ]
#                          [ <cpp-elif-stmt>
#                             [ <block> ]
#                          ]...
#                          [ <cpp-else-stmt>
#                             [ <block> ]
#                          ]
#                          <cpp-endif-stmt>
#     '''
#
#     subclass_names = []
#     use_names = ['Cpp_If_Stmt', 'Block', 'Cpp_Elif_Stmt', 'Cpp_Else']
#
#     @staticmethod
#     def match(string):
#         return BlockBase.match(
#             Cpp_If_Stmt, [Execution_Part_Construct,
#                           Cpp_Elif_Stmt,
#                           Execution_Part_Construct,
#                           Cpp_Else_Stmt,
#                           Execution_Part_Construct],
#             Cpp_Endif_Stmt, string,
#             enable_cpp_if_construct_hook=True)
#
#     def tofortran(self, tab='', isfix=None):
#         tmp = []
#         start = self.content[0]
#         end = self.content[-1]
#         tmp.append(start.tofortran(tab='', isfix=isfix))
#         for item in self.content[1:-1]:
#             if isinstance(item, (Cpp_Elif_Stmt, Cpp_Else_Stmt)):
#                 tmp.append(item.tofortran(tab='', isfix=isfix))
#             else:
#                 tmp.append(item.tofortran(tab=tab+'  ', isfix=isfix))
#         tmp.append(end.tofortran(tab='', isfix=isfix))
#         return '\n'.join(tmp)


class Cpp_If_Stmt(Base):
    '''
    C99 6.10.1 Conditional inclusion

    if-stmt is  # if constant-expression new-line
                | ifdef identifier new-line
                | ifndef identifier new-line

    '''

    subclass_names = []
    use_names = ['Cpp_Macro_Identifier']

    _regex = re.compile(r"#\s*(ifdef|ifndef|if)\b")  # 'if' last, order matters

    @staticmethod
    def match(string):
        if not string:
            return None
        line = string.strip()
        found = Cpp_If_Stmt._regex.match(line)
        if not found:
            return None
        kind = found.group()[1:].strip()
        rhs = line[found.end():].strip()
        if rhs is None or len(rhs) == 0:
            return None
        if kind in ['ifdef', 'ifndef']:
            rhs = Cpp_Macro_Identifier(rhs)
        return (kind, rhs)

    def tostr(self):
        return ('#{0} {1}'.format(self.items[0], self.items[1]))


class Cpp_Elif_Stmt(Base):
    '''
    C99 6.10.1 Conditional inclusion

    elif-stmt is  # elif constant-expression new-line

    '''

    subclass_names = []

    _regex = re.compile(r"#\s*elif\b")

    @staticmethod
    def match(string):
        if not string:
            return None
        line = string.strip()
        found = Cpp_Elif_Stmt._regex.match(line)
        if not found:
            return None
        rhs = line[found.end():].strip()
        if len(rhs) == 0:
            return None
        return (rhs,)

    def tostr(self):
        return ('#elif {0}'.format(self.items[0]))


class Cpp_Else_Stmt(Base):
    '''
    C99 6.10.1 Conditional inclusion

    else-stmt is  # else new-line

    '''

    subclass_names = []

    _regex = re.compile(r"#\s*else\b")

    @staticmethod
    def match(string):
        if not string:
            return None
        line = string.strip()
        found = Cpp_Else_Stmt._regex.match(line)
        if not found:
            return None
        return ()

    def tostr(self):
        return ('#else')


class Cpp_Endif_Stmt(Base):
    '''
    C99 6.10.1 Conditional inclusion

    endif-stmt is  # endif new-line

    '''

    subclass_names = []

    _regex = re.compile(r"#\s*endif\b")

    @staticmethod
    def match(string):
        if not string:
            return None
        line = string.strip()
        found = Cpp_Endif_Stmt._regex.match(line)
        if not found:
            return None
        return ()

    def tostr(self):
        return ('#endif')


class Cpp_Include_Stmt(Base):  # 6.10.2 Source file inclusion
    """
    C99 6.10.2 Source file inclusion

    include_stmt is # include [ <h-char-sequence>
                              | "q-char-sequence"
                              | pp-tokens ] new-line
    """

    _regex = re.compile(r"#\s*include\b")

    use_names = ['Include_Filename']

    @staticmethod
    def match(string):
        '''Implements the matching for an include statement.

        :param str string: the string to match with as an include statement.
        :returns: a tuple of size 1 containing a Cpp_Include_Filename \
        object with the matched filename if there is a match, or None \
        if there is not.
        :rtype: (:py:class:`fparser.two.Fortran2003.Include_Filename`) \
        or NoneType
        '''

        if not string:
            return None
        line = string.strip()
        found = Cpp_Include_Stmt._regex.match(line)
        if not found:
            # The line does not match an include statement
            return None
        rhs = line[found.end():].strip()
        if rhs is None or len(rhs) < 3:
            # Either we didn't find any includes or the content after
            # the include token is too short to be valid (it must at
            # least contain quotes and one character.
            return None
        if not (rhs[0] == '"' and rhs[-1] == '"') and \
           not (rhs[0] == '<' and rhs[-1] == '>'):
            # The filename should be surrounded by double
            # quotes or '<...>' but this is not the case.
            return None
        # Remove the quotes.
        file_name = rhs[1:-1]
        # Pass the potential filename to the relevant class.
        name = Include_Filename(file_name)
        if not name:
            raise InternalError(
                "C99Preprocessor.py:Cpp_Include_Stmt:match Include_Filename "
                "should never return None or an empty name")
        return (name,)

    def tostr(self):
        '''
        :return: this include_stmt as a string
        :rtype: str
        '''
        return '#include "{0}"'.format(self.items[0])


class Cpp_Macro_Stmt(Base):  # 6.10.3 Macro replacement
    """
    C99 6.10.3 Macro replacement

    macro_stmt is # define identifier [( [identifier-list] ) |(...) ]
                  [ replacement-list ] new-line

    Important: No preceding whitespace is allowed for the left parenthesis of
    the optional identifier-list. If a preceding whitespace is encountered,
    the bracket is considered part of the replacement-list
    """

    use_names = ['Cpp_Macro_Identifier', 'Cpp_Macro_Identifier_List']

    _regex = re.compile(r"#\s*define\b")

    @staticmethod
    def match(string):
        if not string:
            return None
        line = string.strip()
        found = Cpp_Macro_Stmt._regex.match(line)
        if not found:
            # The line does not match a define statement
            return None
        rhs = line[found.end():].strip()
        found = pattern.macro_name.match(rhs)
        if not found:
            return None
        name = Cpp_Macro_Identifier(found.group())
        definition = rhs[found.end():]
        # note no strip here because '#define MACRO(x)' and
        # '#define MACRO (x)' are functionally different
        if len(definition) == 0:
            return (name,)
        if definition[0] == '(':
            found = Cpp_Macro_Identifier_List._regex.match(definition)
            if not found:
                # The definition starts with a bracket (without preceding
                # white space) but does not match an identifier list
                return None
            parameter_list = found.group()
            definition = definition[found.end():].strip()
            return (name, parameter_list, definition)
        definition = definition.strip()
        # now that we now it doesn't have a parameter list, we can strip
        if len(definition) > 1:
            return (name, definition)
        else:
            return (name,)

    def tostr(self):
        if len(self.items) > 2:
            return ('#define {0}{1} {2}'.format(self.items[0], self.items[1],
                                                self.items[2]))
        elif len(self.items) > 1:
            return ('#define {0} {1}'.format(self.items[0], self.items[1]))
        else:
            return ('#define {0}'.format(self.items[0]))


class Cpp_Macro_Identifier(StringBase):  # pylint: disable=invalid-name
    '''Implements the matching of a macro identifier.'''

    # There are no other classes. This is a simple string match.
    subclass_names = []

    @staticmethod
    def match(string):
        return StringBase.match(pattern.abs_macro_name, string.strip())


class Cpp_Macro_Identifier_List(Base):
    '''Implements the matching of an identifier list in a macro definition.

    identifier-list is (identifier [, identifier-list | ...])
                       | (...)
    '''

    _regex = re.compile(r'\((\s*[A-Za-z_]\w*'
                        r'(\s*,\s*([A-Za-z_]|\.{3}))*|\.{3})\s*\)')

    @staticmethod
    def match(string):
        if not string:
            return None
        line = string.strip()
        found = Cpp_Macro_Identifier_List._regex.match(line)
        if not found:
            return None
        return (found.group(),)

    def tostr(self):
        return (self.items[0])


class Cpp_Undef_Stmt(Base):
    '''Implements the matching of a preprocessor undef statement for a macro.

    undef-stmt is # undef identifier new-line

    Strictly, this is part of 6.10.3 but since it is identified by a different
    directive keyword (undef instead of define) we treat it separately.
    '''

    subclass_names = []

    use_names = ['Cpp_Macro_Identifier']

    _regex = re.compile(r"#\s*undef\b")

    @staticmethod
    def match(string):
        if not string:
            return None
        line = string.strip()
        found = Cpp_Undef_Stmt._regex.match(line)
        if not found:
            # The line does not match a define statement
            return None
        rhs = line[found.end():].strip()
        found = pattern.macro_name.match(rhs)
        if not found:
            return None
        return(Cpp_Macro_Identifier(found.group()),)

    def tostr(self):
        return ('#undef {0}'.format(self.items[0]))


class Cpp_Line_Stmt(Base):  # 6.10.4 Line control
    """
    C99 6.10.4 Line control

    line-stmt is # line digit-sequence [ "s-char-sequence" ] new-line
                        | pp-tokens new-line
    """

    subclass_names = []

    _regex = re.compile(r"#\s*line\b")

    @staticmethod
    def match(string):
        if not string:
            return None
        line = string.strip()
        found = Cpp_Line_Stmt._regex.match(line)
        if not found:
            return None
        rhs = line[found.end():].strip()
        if len(rhs) == 0:
            return None
        return (rhs,)

    def tostr(self):
        return ('#line {0}'.format(self.items[0]))


class Cpp_Error_Stmt(Base):  # 6.10.5 Error directive
    """
    C99 6.10.5 Error directive

    error-stmt is # error [pp-tokens] new-line

    """

    subclass_names = []

    _regex = re.compile(r"#\s*error\b")

    @staticmethod
    def match(string):
        if not string:
            return None
        line = string.strip()
        found = Cpp_Error_Stmt._regex.match(line)
        if not found:
            return None
        rhs = line[found.end():].strip()
        if len(rhs) == 0:
            return ()
        else:
            return (rhs,)

    def tostr(self):
        if len(self.items) > 0:
            return ('#error {0}'.format(self.items[0]))
        else:
            return ('#error')


class Cpp_Warning_Stmt(Base):
    """
    Not actually part of C99 but supported by most preprocessors and
    with syntax identical to Cpp_Error_Stmt

    warning-stmt is # warning [pp-tokens] new-line

    """

    subclass_names = []
    _regex = re.compile(r"#\s*warning\b")

    @staticmethod
    def match(string):
        if not string:
            return None
        line = string.strip()
        found = Cpp_Warning_Stmt._regex.match(line)
        if not found:
            return None
        rhs = line[found.end():].strip()
        if len(rhs) == 0:
            return ()
        else:
            return (rhs,)

    def tostr(self):
        if len(self.items) > 0:
            return ('#warning {0}'.format(self.items[0]))
        else:
            return ('#warning')


# 6.10.6 Pragma directive
# Pragma Preprocessor directives not implemented since Fortran has its own
# Pragma syntax in the form of comments. For that reason, most preprocessors
# do not support C preprocess pragmas in Fortran code either.


class Cpp_Null_Stmt(Base):  # 6.10.7 Null directive
    """
    C99 6.10.7  Null directive

    null-stmt is # new-line

    """

    subclass_names = []

    @staticmethod
    def match(string):
        if not string:
            return None
        line = string.strip()
        if not line == '#':
            return None
        else:
            return ()

    def tostr(self):
        return '#'
