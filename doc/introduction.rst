.. _Introduction:

Introduction
============

The fparser package is a Python implementation of a Fortran
66/77/90/95/2003 language parser. The code is available on the Python
Package Index (https://pypi.python.org/pypi/fparser) or from github
(https://github.com/stfc/fparser).  Currently fparser actually
includes two different implementations, the  first
supports Fortran 66/77/90 (see :ref:`f90_parser`) while the second  also has
support for Fortran 2003 (see :ref:`Fortran2003`).

The Fortran language syntax rules are defined in `Fortran2003.py`_,
the rules are taken from the following ISO/IEC 1539 working draft:
http://j3-fortran.org/doc/2003_Committee_Draft/04-007.pdf.

.. _Fortran2003.py:  https://github.com/stfc/fparser/blob/master/src/fparser/Fortran2003.py

The original version of this code was developed by Pearu Peterson as a
part of the f2py project (https://github.com/pearu/f2py). It has
subsequently been used and developed in the PSyclone and Habakkuk
(https://github.com/arporter/habakkuk) tools.
