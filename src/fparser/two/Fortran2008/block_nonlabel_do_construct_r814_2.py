from fparser.two.Fortran2003 import (
    Block_Nonlabel_Do_Construct as Block_Nonlabel_Do_Construct_2003,
)
from fparser.two.Fortran2008.nonlabel_do_stmt_r817 import Nonlabel_Do_Stmt


class Block_Nonlabel_Do_Construct(Block_Nonlabel_Do_Construct_2003):
    """Subclass the 2003 version so that this class will import the
    Fortran2008 Nonlabel_Do_Stmt class

    """

    @staticmethod
    def nonlabel_do_stmt_cls():
        """
        :returns: Fortran2008 Nonlabel_Do_Stmt class.
        :rtype: :py:class:`fparser.two.Fortran2008.Nonlabel_Do_Stmt`

        """
        return Nonlabel_Do_Stmt
