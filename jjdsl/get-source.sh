#!/bin/bash
mkdir out
mkdir out/scripts
git clone $1 source
cd source
git switch $2