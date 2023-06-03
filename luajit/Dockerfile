# Latest Ubuntu image
FROM ubuntu:latest

# Make a working directory
WORKDIR /deez
COPY . .

# Basic system-level dependencies
RUN apt-get update && \
    apt install -y make tar unzip gcc software-properties-common git curl build-essential && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y neovim

ARG LUAJIT_VERSION=2.1.0-beta3

RUN curl -L http://luajit.org/download/LuaJIT-${LUAJIT_VERSION}.tar.gz | tar zx && \
    cd ./LuaJIT-${LUAJIT_VERSION} && \
    make && \
    make install && \
    cd .. && rm /LuaJIT-${LUAJIT_VERSION} -rf

# Commands for docker run
CMD make fmt && \
    make lint && \
    make test

# Idk how to use docker 🧐