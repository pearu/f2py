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
fparser2 symbol-table module.

'''

from __future__ import absolute_import, print_function
from enum import Enum
from collections import namedtuple


class SymbolTableError(Exception):
    ''' Base class exception for symbol-table related errors. '''


class SymbolTables(object):
    # pylint: disable=too-many-instance-attributes
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
        result = "SymbolTables contains {0} symbol tables:\n".format(
            len(self._symbol_tables))
        return result

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

        :param str name: the scope name for the new table.

        :returns: the new symbol table.
        :rtype: :py:class:`fparser.two.symbol_table.SymbolTable`

        '''
        full_name = ":".join([table.name for table in self._scope_stack] +
                             [name.lower()])
        table = SymbolTable(full_name)
        self._symbol_tables[full_name] = table
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
        new entry is created in the internal dict of symbol tables.

        the supplied name is not in the stored list of symbol tables then
        a new SymbolTable is created and added to the list.

        :param str name: name of the scoping region.

        :raises SymbolTableError: if the current scope is already within the \
                                  named scope.
        '''
        if not self._scope_stack:
            # We're not already inside a nested scope
            try:
                table = self.lookup(name)
            except KeyError:
                table = self.add(name)
        else:
            # We are already inside a scoping region
            table = SymbolTable(name, parent=self._scope_stack[-1])
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

    '''
    Symbol = namedtuple("Symbol", "name primitive_type kind shape visibility")

    def __init__(self, name, parent=None):
        '''
        :param str name: the name of this scope. Will be the name of the \
                         associated module or routine.
        '''
        self._name = name
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
        symbols = "Symbols:\n" + "\n".join(list(self._symbols.keys()))
        uses = "\nUsed modules:\n" + "\n".join(list(self._modules.keys()))
        return "===========\nTable {0}\n".format(self._name) + symbols + uses

    def new_symbol(self, name, primitive_type, kind=None, shape=None,
                   visibility=SymbolVisibility.PUBLIC):
        '''
        '''
        lname = name.lower()
        self._symbols[lname] = SymbolTable.Symbol(lname, primitive_type, kind,
                                                  shape, visibility)

    def new_module(self, name, only_list=None):
        '''
        '''
        lname = name.lower()
        self._modules[lname] = only_list

    def lookup(self, name):
        '''
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
        '''
        return self._parent

    @parent.setter
    def parent(self, value):
        '''
        '''
        self._parent = value

    def add_child(self, child):
        '''
        '''
        self._children.append(child)

    @property
    def children(self):
        '''
        '''
        return self._children


#: The single, global container for all symbol tables constructed while
#: parsing.
SYMBOL_TABLES = SymbolTables()


__all__ = ["SymbolTables", "SymbolTable", "SYMBOL_TABLES"]
