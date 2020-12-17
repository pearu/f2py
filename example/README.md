# Examples
This directory contains some example scripts that are using fparser.

## fparser2_f2008.py
This is a simple Python script to show that fparser can handle separate
modules and subroutines in one file. It parses a simple Fortran
code specified as a string in the Python script, and then prints the
parsed code to stdout.

## create_dependencies.py
This program prints dependencies between Fortran source files to stdout,
in a format suitable to be used in a Makefile. Usage:

	$ $(PATH_TO_FPARSER)/example/create_dependencies.py *f90
	configuration_mod.o: base_mesh_config_mod.o extrusion_uniform_config_mod.o \
		finite_element_config_mod.o io_utility_mod.o partitioning_config_mod.o \
		perturbation_bell_config_mod.o planet_config_mod.o timestepping_config_mod.o
	write_diagnostics_mod.o: write_methods_mod.o

### Known issues:
- If you do not have ``python`` in your path (e.g. you have only ``python3``
  installed), you have to either invoke the script using:

      python3 $(PATH_TO_FPARSER)/example/create_dependencies.py *.f90
  or modify the first line of the script to use ``python3`` instead of
  ``python``.
- The script will only detect dependencies from the files specified on the
  command line to files in the current directory. It assumes that the name in
  the ``use`` statement is the same as the file name that stores the module.
  For example, if your code has:

  	  use mymodule

  then it will look for any of the files ``mymodule.f90``, ``mymodule.F90``,
  or ``mymodule.x90`` (the latter to support a coding style used in PSyclone).
  If none of these files is found, no dependencies will be printed for this
  case. You can modify the ``create_dependency.py`` script if you are using
  a different naming style for your files (see lines 101 to 107 in the
  script).

### Todo
- Remove the need for a file naming convention by parsing all files, and then
  using this information to find dependencies.
- Support dependencies to other libraries, e.g.:

      $ ./create_dependencies.py -l NETCDF_LIB=/opt/mynetcdf-dir my_netcdf_source.f90
      my_netcdf_source.o: $(NETCDF_LIB)
