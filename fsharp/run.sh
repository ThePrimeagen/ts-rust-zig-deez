#!/usr/bin/env bash

set -e pipefail

docker build -t monkey-fsharp -f Monkey/Dockerfile .
docker run -it --rm monkey-fsharp
