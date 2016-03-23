#!/bin/bash

# This script packages the code into the format required for submission.

set -e

dst="milestone2"
d="$(basename "$(pwd)")"

cd ..

# Delete the output directory and tarball if any
test -e "$dst" && rm -rfv "$dst"
test -e "${dst}.tar.gz" && rm -fv "${dst}.tar.gz"
mkdir "$dst"

# Copy the source code into a subdirectory named `src` and remove the cabal
# sandbox, git files, and built files
cp -vr "$d" "$dst/src"
rm -rf $dst/src/{.cabal-sandbox,cabal.sandbox.config,.git,dist}

sed 's/dist/src\/dist/' \
    < "$dst/src/run_milestone1.sh" \
    > "$dst/run_milestone1.sh"
rm -v "$dst/src/run_milestone1.sh"
chmod +x "$dst/run_milestone1.sh"

sed 's/dist/src\/dist/' \
    < "$dst/src/run_milestone2.sh" \
    > "$dst/run_milestone2.sh"
rm -v "$dst/src/run_milestone2.sh"
chmod +x "$dst/run_milestone2.sh"

# Make symlinks for the valid and invalid programs
mkdir "$dst/programs"
ln -vs "../src/programs/valid" "$dst/programs"
ln -vs "../src/programs/invalid-type" "$dst/programs"
ln -vs "../src/programs/invalid" "$dst/programs"

# Make a symlink for the README
ln -vs "../src/README" "$dst"

# Compile the PDF with the answer to question 3
mkdir "$dst/doc"
cd "$dst/src/tex"
pdflatex milestone1
pdflatex milestone2
cd -
ln -vs ../src/tex/milestone1.pdf "$dst/doc"

# Create the tarball
tar cvzf "${dst}.tar.gz" "$dst/"
