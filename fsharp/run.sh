#!/usr/bin/env bash

set -e pipefail

docker build -t lexer-fsharp -f Lexer/Dockerfile .
docker run -it --rm lexer-fsharp
