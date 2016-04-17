#!/bin/bash

FAILED=0

find programs/valid-run programs/valid-type -type f -name '*.go' |
while read input ; do
    echo "checking $input"
    if ! ./cpp-compile.sh "$input" > compile_out ; then
        echo "$input fails to compile"
        FAILED=$(( FAILED + 1 ))
        break
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

        continue
    fi
done

if [ $FAILED -ne 0 ] ; then
    echo $FAILED "errors occurred"
    exit 1
fi
