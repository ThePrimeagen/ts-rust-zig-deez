# Latest Ubuntu image
FROM ubuntu:latest

# Add language folder to working dir 
COPY . /deez

# Make a working directory
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && \
    apt install -y software-properties-common git curl build-essential && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y neovim

# Commands for docker run
CMD make fmt && \
    make lint && \
    make test

