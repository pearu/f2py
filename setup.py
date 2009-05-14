#!/usr/bin/env python

CLASSIFIERS = """\
Development Status :: 3 - Alpha
Intended Audience :: Developers
Intended Audience :: Science/Research
License :: OSI Approved
Natural Language :: English
Programming Language :: C
Programming Language :: Fortran
Programming Language :: Python
Topic :: Scientific/Engineering
Topic :: Software Development
Topic :: Software Development :: Code Generators
Operating System :: Microsoft :: Windows
Operating System :: POSIX
Operating System :: Unix
Operating System :: MacOS
"""

import os
if os.path.exists('MANIFEST'): os.remove('MANIFEST')

#from distutils.core import Extension

if __name__ == '__main__':
    from distutils.core import setup
    setup(name='f2py_project',
          version='0.1-bzr',
          author = 'Pearu Peterson',
          author_email = 'pearu.peterson@gmail.com',
          license = 'BSD',
          url = 'http://www.f2py.com',
          download_url = 'http://www.f2py.com',
          classifiers=filter(None, CLASSIFIERS.split('\n')),
          description = 'The F2PY Project',
          long_description = '''\
The F2PY project is created to unify the efforts of supporting easy
connection between Fortran and Python languages. The project will
provide software, documentation, and support to solve a difficult task
of automatically generating Python wrappers to Fortran libraries.
The following packages are provided:

  fparser - Fortran 66/77/90/95/2003 parser
  extgen  - extension generation toolbox [NOT IMPLEMENTED]
  f2py    - the third generation of the F2PY tool [NOT IMPLEMENTED]
  
The aim is to have a tool, called f2py, that can automatically
generate Python wrappers to any Fortran program, be it written in
Fortran 66/77 or in Fortran 90/95/2003.
''',
          platforms = ["All"],
          packages = ['fparser',
                      #'extgen', #XXX: to be removed
                      #'srcgen',
                      ],
          keywords          = ['Fortran','f2py', 'source', 'interface',
                               'generation'],
          )

