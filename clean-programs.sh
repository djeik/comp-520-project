#!/bin/bash

for f in $(find programs/benchmark programs/valid-run programs/valid-type -type f -name '*.go') ; do
    EXE=${f%.*}
    if [ -x "$EXE" ] ; then
        rm -v "$EXE"
    fi
done
