#!/bin/bash

set -e

IN="$1"
OUT="${1%.*}"

./run.sh cpp "$IN" > out.cpp
g++ -Iruntime -std=c++11 out.cpp -o "$OUT"
./$OUT
