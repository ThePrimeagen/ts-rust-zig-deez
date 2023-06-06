#!/usr/bin/env bash

# shellcheck disable=SC2155
declare -g PROGDIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" ; pwd )
source "${PROGDIR}"/src/lexer.sh

lexer_scan '
   let five = 5;
   let ten = 10;
   let add = fn(x, y) {
      x + y;
   };
   let result = add(five, ten);
'

# shellcheck disable=SC2086
declare -p ${!TOKEN_*} | sort -Vk3
