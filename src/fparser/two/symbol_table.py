# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author: A. R. Porter, STFC Daresbury Lab

'''
The fparser2 symbol-table module. Defines various classes as well as
the single, global SYMBOL_TABLES instance. The latter is a container
for all of the top-level scoping units encountered during parsing.

'''

from __future__ import absolute_import, print_function
from enum import Enum
from collections import namedtuple


class SymbolTableError(Exception):
    ''' Base class exception for symbol-table related errors. '''


class SymbolTables(object):
    '''
    Class encapsulating functionality for the global symbol-tables object.
    This is a container for all symbol tables constructed while parsing
    code. All names are converted to lower case (since Fortran is not
    case sensitive).

    '''
    def __init__(self):
        self._symbol_tables = {}
        # Those classes that correspond to a new scoping unit
        self._scoping_unit_classes = []
        # The stack of symbol tables accessible in the current scope
        self._scope_stack = []

    def __str__(self):
        result = ("SymbolTables: {0} tables\n"
                  "========================\n".format(
                      len(self._symbol_tables)))
        return result + "\n".join(sorted(self._symbol_tables.keys()))

    def clear(self):
        '''
        Deletes any stored SymbolTables but retains the stored list of
        classes that define scoping units.

        '''
        self._symbol_tables = {}
        self._scope_stack = []

    def add(self, name):
        '''
        Add a new symbol table with the supplied name. The name will be
        converted to lower case if necessary.

        :param str name: the name for the new table.

        :returns: the new symbol table.
        :rtype: :py:class:`fparser.two.symbol_table.SymbolTable`

        :raises SymbolTableError: if there is already an entry with the \
                                  supplied name.
        '''
        lower_name = name.lower()
        if lower_name in self._symbol_tables:
            raise SymbolTableError(
                "The table of top-level (un-nested) symbol tables already "
                "contains an entry for '{0}'".format(lower_name))
        table = SymbolTable(lower_name)
        self._symbol_tables[lower_name] = table
        return table

    def lookup(self, name):
        '''
        Find the named symbol table and return it.

        :param str name: the name of the required symbol table (not case \
                         sensitive).

        :returns: the named symbol table.
        :rtype: :py:class:`fparser.two.symbol_table.SymbolTable`

        '''
        return self._symbol_tables[name.lower()]

    @property
    def scoping_unit_classes(self):
        '''
        :returns: the fparser2 classes that are taken to mark the start of \
                  a new scoping region.
        :rtype: list of types

        '''
        return self._scoping_unit_classes

    @scoping_unit_classes.setter
    def scoping_unit_classes(self, value):
        '''
        Set the list of fparser2 classes that are taken to mark the start of \
        a new scoping region.

        :param value: the list of fparser2 classes.
        :type value: list of types

        '''
        self._scoping_unit_classes = value

    @property
    def current_scope(self):
        '''
        :returns: the symbol table for the current scoping unit or None.
        :rtype: :py:class:`fparser.two.symbol_table.SymbolTable` or NoneType
        '''
        if self._scope_stack:
            return self._scope_stack[-1]
        return None

    def enter_scope(self, name):
        '''
        Called when the parser enters a new scoping region (i.e. when it
        encounters one of the classes listed in `_scoping_unit_classes`).
        Sets the 'current scope' to be the symbol table with the supplied name.
        If there is no existing stack of (nested) scoping regions then a
        new entry is created in the internal dict of symbol tables. If there
        is an existing stack then a new table is created and added to the
        bottom.

        :param str name: name of the scoping region.

        :raises SymbolTableError: if the current scope is already within the \
                                  named scope.
        '''
        lname = name.lower()

        if not self._scope_stack:
            # We're not already inside a nested scope.
            try:
                table = self.lookup(lname)
            except KeyError:
                # Create a new, top-level symbol table with the supplied name.
                table = self.add(lname)
        else:
            # We are already inside a scoping region so create a new table
            # and setup its parent/child connections.
            table = SymbolTable(lname, parent=self._scope_stack[-1])
            self._scope_stack[-1].add_child(table)

        # Finally, put the table on the stack of nested regions
        self._scope_stack.append(table)

    def exit_scope(self):
        '''
        Marks the end of the processing of the current scoping unit.

        '''
        self._scope_stack.pop(-1)


class SymbolVisibility(Enum):
    '''
    Enumeration for the visibility of a symbol within a particular scope.

    '''
    PUBLIC = 1
    PRIVATE = 2


class SymbolTable(object):
    '''
    Class implementing a single symbol table.

    :param str name: the name of this scope. Will be the name of the \
                     associated module or routine.
    :param parent: the symbol table within which this one is nested (if any).
    :type parent: :py:class:`fparser.two.symbol_table.SymbolTable.Symbol`

    '''
    Symbol = namedtuple("Symbol", "name primitive_type kind shape visibility")

    def __init__(self, name, parent=None):
        self._name = name.lower()
        # Symbols defined in this scope.
        self._symbols = {}
        # Modules imported into this scope.
        self._modules = {}
        # Reference to a SymbolTable that contains this one (if any). Actual
        # value (if any) is set via setter method.
        self._parent = None
        self.parent = parent
        # Symbol tables nested within this one.
        self._children = []

    def __str__(self):
        header = "===========\n"
        symbols = "Symbols:\n"
        if self._symbols:
            symbols += "\n".join(list(self._symbols.keys())) + "\n"
        uses = "Used modules:\n"
        if self._modules:
            uses += "\n".join(list(self._modules.keys())) + "\n"
        return ("{0}Symbol Table '{1}'\n".format(header, self._name) +
                symbols + uses + header)

    def new_symbol(self, name, primitive_type, kind=None, shape=None,
                   visibility=SymbolVisibility.PUBLIC):
        '''
        Creates a new Symbol with the specified properties and adds it to
        the symbol table. The supplied name is converted to lower case.

        :param str name: the name of the symbol.
        :param primitive_type:
        :param kind:
        :param shape:
        :param visibility:
        '''
        lname = name.lower()
        self._symbols[lname] = SymbolTable.Symbol(lname, primitive_type, kind,
                                                  shape, visibility)

    def new_module(self, name, only_list=None):
        '''
        Creates an entry in the table for the USE of a module with the supplied
        name.

        :param str name: the name of the module being imported via a USE. Not \
            case sensitive.
        :param only_list: Whether or not there is an 'only:' clause on the \
            USE statement and, if so, the names of the symbols being imported.
        :type only_list: NoneType or list of str

        '''
        lname = name.lower()
        self._modules[lname] = only_list

    def lookup(self, name):
        '''
        Lookup the symbol with the supplied name.

        :param str name: the name of the symbol to lookup (not case sensitive).

        :returns: the named symbol.
        :rtype: :py:class:`fparser.two.symbol_table.SymbolTable.Symbol`

        :raises KeyError: if the named symbol cannot be found in this or any \
                          parent scope.
        '''
        # Fortran is not case sensitive so convert input to lowercase.
        lname = name.lower()
        if lname in self._symbols:
            return self._symbols[lname]
        # No match in this scope - search in parent scope (if any)
        if self.parent:
            return self.parent.lookup(lname)
        raise KeyError("Failed to find symbol named '{0}'".format(lname))

    @property
    def name(self):
        '''
        :returns: the name of this symbol table (scoping region).
        :rtype: str
        '''
        return self._name

    @property
    def parent(self):
        '''
        :returns: the parent symbol table (scoping region) that contains \
                  this one (if any).
        :rtype: :py:class:`fparser.two.symbol_table.SymbolTable` or NoneType
        '''
        return self._parent

    @parent.setter
    def parent(self, value):
        '''
        Set the parent scope for this symbol table.

        :param value: the parent symbol table.
        :type value: :py:class:`fparser.two.symbol_table.SymbolTable`

        '''
        self._parent = value

    def add_child(self, child):
        '''
        Adds a child symbol table (scoping region nested within this one).

        :param child: the nested symbol table.
        :type child: :py:class:`fparser.two.symbol_table.SymbolTable`

        :raises TypeError: if the supplied child is not a SymbolTable.

        '''
        if not isinstance(child, SymbolTable):
            raise TypeError("Expected a SymbolTable instance but got '{0}'".
                            format(type(child).__name__))
        self._children.append(child)

    @property
    def children(self):
        '''
        :returns: the child (nested) symbol tables, if any.
        :rtype: list of :py:class:`fparser.two.symbol_table.SymbolTable`
        '''
        return self._children


#: The single, global container for all symbol tables constructed while
#: parsing.
SYMBOL_TABLES = SymbolTables()


__all__ = ["SymbolTableError", "SymbolTables", "SymbolTable", "SYMBOL_TABLES"]
