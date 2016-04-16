#!/bin/bash

# Checks that all the programs that are supposed to run compile and run and
# output their expected output.

FAILED=0

for input in $(find programs/valid-run -type f -name '*.go') ; do
    echo "checking $input"
    if ! ./compile.sh "$input" ; then
        echo "$input fails to compile"
        FAILED=$(( FAILED + 1 ))
        continue
    fi

    EXPECTED="$(
        sed -n '/^\/\//p' < "$input" |
        cut -d " " -f2- |
        while read line ; do
            if [ "$line" = "//." ] ; then
                break
            else
                echo "$line"
            fi
        done
    )"

    ACTUAL="$(./${input%.*})"

    if [ "$EXPECTED" != "$ACTUAL" ] ; then
        echo "$input gives the wrong output"
        echo "expected:"
        echo "$EXPECTED"
        echo ""
        echo "actual:"
        echo "$ACTUAL"
        FAILED=$(( FAILED + 1 ))
    fi
done

if [ $FAILED -ne 0 ] ; then
    echo $FAILED "errors occurred"
    exit 1
fi
