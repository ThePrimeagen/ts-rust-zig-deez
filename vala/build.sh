#!/usr/bin/env bash

set -xe

valac -o test-vala --save-temps -X -Wno-incompatible-pointer-types src/main.vala
