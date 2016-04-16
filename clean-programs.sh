#!/bin/bash

for f in $(find programs/valid-run -type f -name '*.go') ; do
    EXE=${f%.*}
    if [ -x "$EXE" ] ; then
        rm -v "$EXE"
    fi
done
