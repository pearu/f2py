# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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

"""
    Module containing Fortran2008 Connect_Spec rule R905
"""
from fparser.two.Fortran2003 import (
    Connect_Spec as Connect_Spec_2003,
    File_Unit_Number,
    Label,
    File_Name_Expr,
    Iomsg_Variable,
    File_Unit_Number,
)
from fparser.two.utils import KeywordValueBase, NoMatchError


class Connect_Spec(Connect_Spec_2003):
    """
    Fortran2008 rule R905.

    connect-spec is [ UNIT = ] file-unit-number
                     or ACCESS = scalar-default-char-expr
                     or ACTION = scalar-default-char-expr
                     or ASYNCHRONOUS = scalar-default-char-expr
                     or BLANK = scalar-default-char-expr
                     or DECIMAL = scalar-default-char-expr
                     or DELIM = scalar-default-char-expr
                     or ENCODING = scalar-default-char-expr
                     or ERR = label
                     or FILE = file-name-expr
                     or FORM = scalar-default-char-expr
                     or IOMSG = iomsg-variable
                     or IOSTAT = scalar-int-variable
                     or NEWUNIT = scalar-int-variable
                     or PAD = scalar-default-char-expr
                     or POSITION = scalar-default-char-expr
                     or RECL = scalar-int-expr
                     or ROUND = scalar-default-char-expr
                     or SIGN = scalar-default-char-expr
                     or STATUS = scalar-default-char-expr

    R906 file-name-expr is scalar-default-char-expr
    R907 iomsg-variable is scalar-default-char-variable
    C903 No specifier shall appear more than once in a given connect-spec-list.

    C904 (R904) If the NEWUNIT= specifier does not appear, a file-unit-number
         shall be specified; if the optional characters UNIT= are omitted, the
         file-unit-number shall be the first item in the connect-spec-list.

    C905 (R904) The label used in the ERR= specifier shall be the statement label
         of a branch target statement that appears in the same inclusive scope as
         the OPEN statement.

    C906 (R904) If a NEWUNIT= specifier appears, a file-unit-number shall not
         appear.

    The constraints listed above are checked for in the Open_Stmt.match() method
    as we don't have access to the full list of Connect_Spec elements here.
    The exceptions are the second part of C904 (un-named file-unit-number must
    be first in the list) and C905: these are not currently checked.

    """

    subclass_names = []
    use_names = [
        "File_Unit_Number",
        "Scalar_Default_Char_Expr",
        "Label",
        "File_Name_Expr",
        "Iomsg_Variable",
        "Scalar_Int_Expr",
        "Scalar_Int_Variable",
    ]

    @staticmethod
    def match(string):
        """
        :param str string: Fortran code to check for a match

        :returns: 2-tuple containing the keyword and value or None if the
                  supplied string is not a match
        :rtype: Optional[Tuple[str, Any]]
        """
        # Avoid circular dependencies by importing here.
        # pylint: disable=import-outside-toplevel
        from fparser.two.Fortran2008 import (
            Scalar_Default_Char_Expr,
            Scalar_Int_Variable,
            Scalar_Int_Expr,
        )

        if "=" not in string:
            # The only argument which need not be named is the unit number
            return "UNIT", File_Unit_Number(string)
        # We have a keyword-value pair. Check whether it is valid...
        for keyword, value in [
            (
                [
                    "ACCESS",
                    "ACTION",
                    "ASYNCHRONOUS",
                    "BLANK",
                    "DECIMAL",
                    "DELIM",
                    "ENCODING",
                    "FORM",
                    "PAD",
                    "POSITION",
                    "ROUND",
                    "SIGN",
                    "STATUS",
                ],
                Scalar_Default_Char_Expr,
            ),
            ("ERR", Label),
            ("FILE", File_Name_Expr),
            ("IOSTAT", Scalar_Int_Variable),
            ("IOMSG", Iomsg_Variable),
            ("RECL", Scalar_Int_Expr),
            ("UNIT", File_Unit_Number),
            ("NEWUNIT", File_Unit_Number),
        ]:
            try:
                obj = KeywordValueBase.match(keyword, value, string, upper_lhs=True)
            except NoMatchError:
                obj = None
            if obj is not None:
                return obj
        return None
