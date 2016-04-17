#!/bin/bash

set -e

IN="$1"
CPP_FILE="${1%.*}.cpp"
OUT="${1%.*}"

./run.sh cpp "$1" > "$CPP_FILE"
g++ -Iruntime -std=c++11 "$CPP_FILE" -o "$OUT"
