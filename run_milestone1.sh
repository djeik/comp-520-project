#!/bin/bash
in="$1"
out="${in//.go/.pretty.go}"
exec dist/build/goto/goto pretty "$1" > "$out"
