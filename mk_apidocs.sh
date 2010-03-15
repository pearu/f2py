#!/bin/sh

HTML_TARGET=html
rm -rf doc/_build/ doc/source/generated $HTML_TARGET/*
echo `date -u` ": documentation is being updated... try again in few minutes." >  $HTML_TARGET/index.html
PYTHONPATH=`pwd`:$PYTHONPATH
cd doc && make html || exit 1
cd -
exit 0

