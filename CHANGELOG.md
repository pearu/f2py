# Change log for the fparser package #

Original code by Pearu Peterson.

Modifications by:

A. R. Porter and R. W. Ford of the Science
and Technology Facilities Council, UK.
P. Vitt, University of Siegen, Germany
I. Kavcic, UK Met Office.
J. Henrichs, Australia Bureau of Meteorology

24/11/2017 PR #54. Add support for array initialisation using square
	   brackets in fparser1, e.g. 'integer :: a(2) = [x, y]'

09/11/2017 Issue #40 and PR #56 bug fix for missing comma when
           generating "USE, intrinsic :: ..."

21/10/2017 issue #36 and PR #47 generate correct open statement in
           fparser2 when the 'UNIT' keyword is not provided

20/10/2017 PR #49 pep8, pylint and nose->pytest changes for test_parse.py

20/10/2017 issue #43 and PR #48 add support for Python 3.

19/10/2017 issue #45 and PR #46 - bug fix for optional '::'
           between MODULE PROCEDURE and <procedure name>

13/10/2017 issue #41 and PR #42 - removed __init__.py files from
           directories that do not contain package code

28/09/2017 issue #35 pr #39 add support for the 'opened' option
	   in the inquire statement in fparser2.

06/09/2017 issue #34 pr #37 Fix a format statement parsing
	   bug in fparser2

## Release 0.0.5 (20/06/2017) ##

20/06/2017 issue #30 pr #31 Extend fparser1 to support
 	   Fortran2003 procedure declarations

## Release 0.0.4 ##

11/05/2017 pypi configuration was incorrect so 0.0.3 was
           released on github but not on pypi. 0.0.4 has the
           same functionality as 0.0.3 but is available on
           both pypi and github.

## Release 0.0.3 ##

11/05/2017 #27 Fix a bug in fparser1 to support (Fortran2003)
           class declarations e.g. CLASS(my_class) :: var

## Release 0.0.2 ##

22/03/2017 #11 Configure Travis to automatically release to
           pypi when a tag is created. Create release 0.0.2.

03/03/2017 #5 Extend fparser1 to support calls to type-bound
           procedures when accessed via array elements.

24/02/2017 #4 Extend fparser1 to support the (Fortran2003)
           SELECT TYPE block.

09/02/2017 #2 Create initial documentation, mostly based on
	   Pearu's previous work.

07/02/2017 #10 Merge in fixes to Fortran2003 parser originally
  	   done as part of Habakkuk development.

## Release 0.0.1 ##

31/01/2017 #6 Create first release and upload to pypi.

31/01/2017 #7 Apply regex bug-fix to 'entry' parsing so that
	   all tests pass.

16/01/2017 #1 Initial import of parser code extracted from
	   the f2py project.
