#!/bin/bash
out="${in//.go/.pretty.go}"
exec dist/build/goto/goto pretty "$@" > "$out"
