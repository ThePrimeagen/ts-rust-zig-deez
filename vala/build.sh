#!/usr/bin/env bash

set -xe

rm -f src/*.c monkey.h monkey

valac \
  -o monkey \
  --save-temps \
  --Xcc -I. \
  --Xcc -Wno-incompatible-pointer-types \
  --header monkey.h \
  src/lexer.vala \
  src/main.vala \
  src/token.vala
