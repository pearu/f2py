from fparser.two.Fortran2003 import (
    Block_Label_Do_Construct as Block_Label_Do_Construct_2003,
)
from fparser.two.Fortran2008.label_do_stmt_r816 import Label_Do_Stmt


class Block_Label_Do_Construct(Block_Label_Do_Construct_2003):
    """Subclass the 2003 version so that this class will import the
    Fortran2008 Label_Do_Stmt class

    """

    @staticmethod
    def label_do_stmt_cls():
        """
        :returns: Fortran2008 Label_Do_Stmt class.
        :rtype: :py:class:`fparser.two.Fortran2008.Label_Do_Stmt`

        """
        return Label_Do_Stmt
