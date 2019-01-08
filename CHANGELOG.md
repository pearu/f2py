# Change log for the fparser package #

Original code by Pearu Peterson.

Modifications by (in alphabetical order):

* R. W. Ford, Science & Technology Facilities Council, UK
* M. Hambley, UK Met Office
* J. Henrichs, Australia Bureau of Meteorology
* I. Kavcic, UK Met Office
* A. R. Porter, Science & Technology Facilities Council, UK
* P. Vitt, University of Siegen, Germany

08/01/2019 PR #152 for #142. Bug fixes for the Fortran 2003 forall header
           rule (r754).

07/01/2019 PR #158 for #157. Adds support for a Defined Operator (r311 and
           r312).

07/01/2019 PR #141 for issue #129. Adds extension to support the use of 'x'
           without a preceeding integer in format strings.

20/12/2018 PR #149 for issue #147. Bug fix that corrects the name of one
	   of the subclasses (Associate_Construct) of Executable_Construct.

20/12/2018 PR #151 for issue #150 (and part of PR #142). Removes the
	   triggering of Coveralls from Travis (because it often incorrectly
	   reports that coverage has decreased).

## Release 0.0.8 (03/12/2018) ##

28/11/2018 PR #131 for issue #106. Bug fixes for Procedure_Binding and
           Char_Literal_Constant classes.

28/11/2018 PR #133 for issue #130. Bug fix for handing of derived-type
           statements.

28/11/2018 PR #134 for issue #119. Bug fix for parsing files that contain
           nothing or just white space.

23/11/2018 PR #122 for issue #118. Bug fix for reporting invalid
           Fortran when parsing `use module_name, only:` in fparser2.

21/11/2018 PR #127 for #126. Adds get_child function to help AST traversal.

19/11/2018 PR #124 for #112. Bug fix - spaces within names are now
           rejected by fparser2.

15/11/2018 PR #123 for #117, Fixes a bug that caused fparser to crash
           for code where a PROGRAM statement was missing an associated
           name.

14/11/2018 PR #121 for #120. Fixes bug in BlockBase such that name
           matches are no longer case sensitive. Improves error
           handling.

08/10/2018 PR #111 - bug fix for #110. Adds support for `kind(my_var)`
           inside a kind expression for a variable declaration.

03/09/2018 PR #100 for issue #99. Adds new SyntaxError to fparser2 and
           fixes bug so that errors are reported correctly for multiple
           program units in a single source file.

08/08/2018 PR #96, for issue #95. Removes dependency on nose and
           tidy test_fortran2003.py for pylint.

01/08/2018 PR #94 for issue #92. Re-structures fparser2 and introduces
           a parser factory. This creates a parser for a specified
           Fortran dialect (currently 2003 or 2008). Intoduces support
           for Fortran2008 submodules.

09/07/2018 PR #90 for issue #89. Make fparser2 pycodestyle conformant.

09/07/2018 PR #88 for issue #81. Bug fix for undefined variable in fparser
           class 'HasImplicitStmt'.

07/07/2018 PR #71 for issue #68. Adds support for keeping input comments
           in the Fortran output for parser 2.

27/06/2018 PR #85 for issue #82. Adds support for the full list of possible
           procedure attributes (POINTER and PROTECTED were missing).

19/06/2018 PR #86 and issue #83. Adds support for the full list of possible
           procedure specifications (MODULE and IMPURE were missing).

## Release 0.0.7 (23/04/2018) ##

20/04/2018 PR #78 and issue #74. Allow the use of the fparser cache to
	   be controlled and disable its use by default.

19/04/2018 PR #70. Re-organise module structure in order to split
	   versions 1 and 2 of the parser.

23/03/2018 PR #75. Allow FortranFileReader to accept either a filename
           or a file handle. Many pylint/pep8 improvements.

20/03/2018 PR #77 and issue #76. Fix bug in ALLOCATE statement when
	   using names of derived types as type specifiers.

05/03/2018 PR #73. Improvements to SourceInfo so that it can take either
	   a filename or a file handle.

26/02/2018 PR #72 and issue #69. Fixes for bugs found when using fparser2
	   to parse and then re-generate LFRic code.

15/01/2018 PR #67. Move old testing code from source files into test
	   framework.

12/01/2018 PR #66 and issue #64. Fix bug where = in a string was being
           treated like an assignment, causing parse errors in some
           format statements

08/01/2018 PR #65 and issue #59. fparser no longer presumes to set-up
           logging - this is left to the master application.

## Release 0.0.6 (04/12/2017) ##

04/12/2017 PRs #61 and #62. Remove the dependence on numpy.

24/11/2017 PR #60. Add fparser2 support for the 'deferred' attribute on a
           procedure declaration.

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
