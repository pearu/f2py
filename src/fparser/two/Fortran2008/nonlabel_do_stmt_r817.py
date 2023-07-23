from fparser.two.Fortran2003 import Nonlabel_Do_Stmt as Nonlabel_Do_Stmt_2003
from fparser.two.Fortran2008 import Loop_Control


class Nonlabel_Do_Stmt(Nonlabel_Do_Stmt_2003):
    """Subclass the 2003 version so that this class will import the
    Fortran2008 Loop_Control class

    """

    @staticmethod
    def loop_control_cls():
        """
        :returns: Fortran2003 Loop_Control class.
        :rtype: :py:class:`fparser.two.Fortran2003.Loop_Control`

        """
        return Loop_Control
