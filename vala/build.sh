#!/usr/bin/env bash

set -xe

rm -f src/*.c monkey

valac \
  -o monkey \
  --save-temps \
  --Xcc -Wno-incompatible-pointer-types \
  src/lexer.vala \
  src/main.vala \
  src/token.vala
