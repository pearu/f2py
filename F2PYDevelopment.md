# Introduction #

The F2PY project uses in its development the Mercurial software configuration management tool. This document collects commands for various development tasks.

# F2PY repository #

To get a local copy of F2PY software, use
```
hg clone https://f2py.googlecode.com/hg/ f2py  
```
Read [A tutorial on using Mercurial](http://mercurial.selenic.com/wiki/Tutorial).

**TODO:** Describe a suggested workflow for using F2PY repository.

# F2PY API Documentation #

To build F2PY API documentation, run in the local `f2py` repository
```
tools/mk_apidocs.sh
```
The API documentation will be available in `html` directory.

To upload F2PY API documentation to http://f2py.sourceforge.net/docs/ and if you are Pearu, then run
```
tools/upload_apidocs.sh
```