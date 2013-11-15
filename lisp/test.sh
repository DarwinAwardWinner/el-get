#!/bin/bash

cd $(dirname $0)

CMD="emacs -Q -batch -f toggle-debug-on-error -l el-get.el"
if [ -n "$1" ]; then
    CMD="$CMD -l $1"
fi

rm -rvf `pwd`/tmp
mkdir -vp `pwd`/tmp
HOME=`pwd`/tmp $CMD
