# Latest Ubuntu image
FROM ubuntu:latest

# Make a working directory
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && \
    apt install -y software-properties-common git curl build-essential && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y neovim

# Extra dependencies
RUN curl -fsS https://dlang.org/install.sh | bash -s dmd
RUN apt-get install -y dub

dub run dfmt -- -h
dub run dscanner -- -h

# Commands for docker run
CMD make fmt && \
    make lint && \
    make test

