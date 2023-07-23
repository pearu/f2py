from fparser.two.Fortran2003 import (
    Action_Term_Do_Construct as Action_Term_Do_Construct_2003,
)
from fparser.two.Fortran2008.label_do_stmt_r816 import Label_Do_Stmt


class Action_Term_Do_Construct(Action_Term_Do_Construct_2003):
    """Subclass the 2003 version so that this class will import the
    Fortran2008 Label_Do_Stmt class

    """

    @staticmethod
    def label_do_stmt_cls():
        """
        :returns: Fortran2008 Label_Do_Stmt class.
        :rtype: :py:class:`fparser.two.Fortran2003.Label_Do_Stmt`

        """
        return Label_Do_Stmt
