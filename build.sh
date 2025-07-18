#!/usr/bin/bash

warnings="          -Wno-unused-function"
warnings="$warnings -Wno-initializer-overrides"
warnings="$warnings -Wno-gnu-zero-variadic-macro-arguments"
warnings="$warnings -Wno-unused-parameter"
warnings="$warnings -Wno-varargs"
warnings="$warnings -Wno-incompatible-pointer-types"

clang src/el.c -o el -Wall -Wextra -pedantic $warnings -g -O1
