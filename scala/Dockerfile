# Latest Ubuntu image
FROM ubuntu:latest

# Make a working directory
WORKDIR /deez

COPY lexer.scala /deez
COPY lexer.test.scala /deez
COPY .scalafmt.conf /deez
COPY Makefile /deez
COPY make.cmd /deez

# Basic system-level dependencies
RUN apt-get update && \
    apt install -y software-properties-common git curl build-essential && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y neovim curl && \
    curl -s --compressed "https://virtuslab.github.io/scala-cli-packages/KEY.gpg" | apt-key add - && \
    curl -s --compressed -o /etc/apt/sources.list.d/scala_cli_packages.list "https://virtuslab.github.io/scala-cli-packages/debian/scala_cli_packages.list" && \
    apt-get update && \
    apt-get install -y scala-cli

# Commands for docker run
CMD make fmt && \
    make lint && \
    make test

