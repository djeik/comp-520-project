#!/bin/bash

set -e

ARCH=macho64
AS=yasm
CC=gcc
GOTO=dist/build/goto/goto
CFLAGS="-Xlinker -no_pie"
ASFLAGS="-f $ARCH"

IN="$1"
ASM_FILE="${IN%.*}.asm"
O_FILE="${IN%.*}.o"
OUT="${IN%.*}"

test -e "$1" || eval "echo \"usage: $0 [FILE]\" ; exit 1"

# build the runtime, if necessary
make -C runtime > /dev/null

# generate the assembly
$GOTO pretty < "$IN" > "$ASM_FILE"

# assemble it to produce an object file
$AS $ASFLAGS "$ASM_FILE" -o "$O_FILE"

# link the object file with the runtime and the c standard library
$CC $CFLAGS $O_FILE runtime/goto.o -o "$OUT"
