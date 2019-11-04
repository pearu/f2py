#!/usr/bin/env python

# Modified work Copyright (c) 2017 Science and Technology Facilities Council
# Modified work Copyright (c) 2017 by J. Henrichs, Bureau of Meteorology
# Original work Copyright (c) 1999-2008 Pearu Peterson

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

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

"""
Defines LineSplitter and helper functions.

Original Author: Pearu Peterson <pearu@cens.ioc.ee>
First version created: May 2006

-----
"""


import re
import six

class String(six.text_type):
    ''' Dummy string class. '''


class ParenString(six.text_type):
    ''' Class representing a parenthesis string. '''


__all__ = ['String', 'string_replace_map', 'splitquote', 'splitparen']

_f2py_str_findall = re.compile(r"_F2PY_STRING_CONSTANT_\d+_").findall
_is_name = re.compile(r'\w*\Z', re.I).match
_is_simple_str = re.compile(r'\w*\Z', re.I).match
_f2py_findall = re.compile(
    r'(_F2PY_STRING_CONSTANT_\d+_|F2PY_EXPR_TUPLE_\d+)').findall


class string_replace_dict(dict):
    """
    Dictionary object that is callable for applying map returned
    by string_replace_map() function.
    """
    def __call__(self, line):
        for k in _f2py_findall(line):
            line = line.replace(k, self[k])
        return line


def string_replace_map(line, lower=False,
                       _cache={'index': 0, 'pindex': 0}):
    """
    1) Replaces string constants with symbol `'_F2PY_STRING_CONSTANT_<index>_'`
    2) Replaces (expression) with symbol `(F2PY_EXPR_TUPLE_<index>)`
    Returns a new line and the replacement map.
    """
    items = []
    string_map = string_replace_dict()
    rev_string_map = {}
    for item in splitquote(line, lower=lower)[0]:
        if isinstance(item, String) and not _is_simple_str(item[1:-1]):
            key = rev_string_map.get(item)
            if key is None:
                _cache['index'] += 1
                index = _cache['index']
                key = "_F2PY_STRING_CONSTANT_%s_" % (index)
                it = item[1:-1]
                string_map[key] = it
                rev_string_map[it] = key
            items.append(item[0]+key+item[-1])
        else:
            items.append(item)
    newline = ''.join(items)
    items = []
    expr_keys = []
    for item in splitparen(newline):
        if isinstance(item, ParenString) and not _is_name(item[1:-1].strip()):
            key = rev_string_map.get(item)
            if key is None:
                _cache['pindex'] += 1
                index = _cache['pindex']
                key = 'F2PY_EXPR_TUPLE_%s' % (index)
                it = item[1:-1].strip()
                string_map[key] = it
                rev_string_map[it] = key
                expr_keys.append(key)
            items.append(item[0]+key+item[-1])
        else:
            items.append(item)
    found_keys = set()
    for k in expr_keys:
        v = string_map[k]
        l = _f2py_str_findall(v)
        if l:
            found_keys = found_keys.union(l)
            for k1 in l:
                v = v.replace(k1, string_map[k1])
            string_map[k] = v
    for k in found_keys:
        del string_map[k]
    return ''.join(items), string_map


def splitquote(line, stopchar=None, lower=False, quotechars='"\''):
    """
    Fast LineSplitter
    """
    items = []
    i = 0
    while 1:
        try:
            char = line[i]
            i += 1
        except IndexError:
            break
        l = []
        l_append = l.append
        nofslashes = 0
        if stopchar is None:
            # search for string start
            while 1:
                if char in quotechars and not nofslashes % 2:
                    stopchar = char
                    i -= 1
                    break
                if char == '\\':
                    nofslashes += 1
                else:
                    nofslashes = 0
                l_append(char)
                try:
                    char = line[i]
                    i += 1
                except IndexError:
                    break
            if not l:
                continue
            item = ''.join(l)
            if lower:
                item = item.lower()
            items.append(item)
            continue
        if char == stopchar:
            # string starts with quotechar
            l_append(char)
            try:
                char = line[i]
                i += 1
            except IndexError:
                if l:
                    item = String(u''.join(l))
                    items.append(item)
                break
        # else continued string
        while 1:
            if char == stopchar and not nofslashes % 2:
                l_append(char)
                stopchar = None
                break
            if char == '\\':
                nofslashes += 1
            else:
                nofslashes = 0
            l_append(char)
            try:
                char = line[i]
                i += 1
            except IndexError:
                break
        if l:
            item = String(u''.join(l))
            items.append(item)
    return items, stopchar


def splitparen(line, paren_open="([", paren_close=")]"):
    """
    Splits a line into top-level parenthesis and not-parenthesised
    parts. E.g.: "a( (1+2)*3) = b(x)" becomes:
    ["a", "( (1+2)*3)", " = b", "(x)"]
    :param str line: the string to split.
    :param str paren_open: The characters that define an open parentheses.
    :param str paren_close: The characters that define a closing parentheses.
    :return: List of parenthesised and not-parenthesised parts
    :rtype: list of str
    The paren_open and paren_close strings must be matched in order:
    paren_open[x] is closed by paren_close[x].
    """

    assert len(paren_open) == len(paren_close)

    items = []   # Result list
    num_backslashes = 0   # Counts consecutive "\" characters
    # Empty if outside quotes, or set to the starting (and therefore
    # also the ending) quote character while reading text inside quotes.
    inside_quotes_char = ""
    start = 0    # Index of start of current part.
    stack = []   # Stack keeping track of required closing brackets

    for idx, char in enumerate(line):
        if char == "\\":
            num_backslashes = (num_backslashes+1) % 2
            continue

        # We had an odd number of \, so the next character is neither
        # a real quote or parenthesis character, and can just be added.
        if num_backslashes == 1:
            num_backslashes = 0
            continue

        # If we are reading a quote, keep on reading till closing
        # quote is reached
        if inside_quotes_char != '':
            # Reset inside_quotes_char if we find the closing quote
            if char == inside_quotes_char:
                inside_quotes_char = ''
            continue

        if char == "\'" or char == '"':
            inside_quotes_char = char
            continue

        pos = paren_open.find(char)
        if pos > -1:
            if len(stack) == 0:
                # New part starts:
                items.append(line[start:idx])
                start = idx
            stack.append(paren_close[pos])
            continue

        # Found closing bracket
        if len(stack) > 0 and char == stack[-1]:
            stack.pop()
            if len(stack) == 0:
                # Found last closing bracket
                items.append(ParenString(line[start:idx+1]))
                start = idx+1

    # Add any leftover characters as a separate item
    if start != len(line):
        items.append(line[start:])
    return items
