#!/bin/bash

# Compiles source files.
./build.sh

# Compiles test files.
./build.sh test

cd ebin

echo -e "\nRunning tests\n"

# Runs all test files.
for filename in *_spec.beam; do
  erl -s $(basename $filename .beam) test -s init stop
done

echo -e "\n\nTerminating tests\n"
