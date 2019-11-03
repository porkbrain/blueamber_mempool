#!/bin/bash

# Directory of Erlang files to build.
dir=${1-src}

# Creates a ebin directory if doesn't exist.
mkdir -p ebin

echo -e "\nCompiling $dir ...\n"

# Compile all Erlang files into the ebin directory.
for filename in $(find $dir -name "*.erl"); do
  erlc -o ebin $filename
done
