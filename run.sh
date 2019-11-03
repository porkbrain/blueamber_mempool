#!/bin/bash

# Recompiles the ebin directory by default.
compile='true'

# Module with boot point start/0.
main='main'

# Parses flags.
while getopts 'sb:' flag; do
  case "${flag}" in
    s) compile='false' ;;
    b) main="${OPTARG}" ;;
    *) error "Unknown flag ${flag}." ;;
  esac
done

# Recompiles unless explicitly asked not to.
if [ $compile = true ]
then
  ./build.sh
fi

# Opens ebin directory.
cd ebin

echo -e "\nRunning\n"

# Calls the $main module's start/0 function.
erl -s $main start -s init stop

echo -e "\n\nTerminating\n"
