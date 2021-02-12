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

    def __str__(self):
        return repr(self.value)


class SymbolTables(object):
    # pylint: disable=too-many-instance-attributes
    '''
    Class encapsulating functionality for the global symbol-tables object.

    '''
    # Class variable to store the singleton instance
    _instance = None

    @staticmethod
    def get():
        '''
        Static function that if necessary creates and returns the singleton
        SymbolTables instance.

        :returns: the singleton SymbolTables instance.
        :rtype: :py:class:`fparser.two.symbol_table.SymbolTables`

        '''
        if not SymbolTables._instance:
            SymbolTables._instance = SymbolTables()
        return SymbolTables._instance

    def __init__(self):
        '''
        The SymbolTables instance is a singleton, and as such will test that
        no instance already exists and raise an exception otherwise.

        :raises SymbolTableError: If a singleton instance of SymbolTables \
                                  already exists.
        '''

        if SymbolTables._instance is not None:
            raise SymbolTableError("Only one instance of "
                                   " SymbolTables can be created")

        self._symbol_tables = {}

    def add(self, name, table):
        '''
        :param str name: the scope name for the new table.
        :param table: the symbol table associated with the named scope.
        :type table: :py:class:`fparser.two.symbol_table.SymbolTable`

        '''
        self._symbol_tables[name] = table


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
    Symbol = namedtuple("Symbol", "primitive_type kind shape visibility")

    def __init__(self, name):
        '''
        :param str name: the name of this scope. Will be the name of the \
                         associated module or routine.
        '''
        self._name = name
        # Symbols defined in this scope.
        self._symbols = {}
        # Modules imported into this scope.
        self._modules = {}

    def new_symbol(self, name, primitive_type, kind=None, shape=None,
                   visibility=SymbolVisibility.PUBLIC):
        '''
        '''
        self._symbols[name] = SymbolTable.Symbol(primitive_type, kind, shape,
                                                 visibility)

    def new_module(self, name, only_list=None):
        '''
        '''
        self._modules[name] = only_list
