# Change log for the fparser package #

Original code by Pearu Peterson.
Modifications by Andrew Porter and Rupert Ford of the Science
and Technology Facilities Council, UK.

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
