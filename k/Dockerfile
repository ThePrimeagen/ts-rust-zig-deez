# Latest Ubuntu image
FROM ubuntu:latest

# Make a working directory, as it would be binded with the current language one
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && apt install -y make git curl && \
    # TODO: these are examplary, change to those you need:
    apt install -y software-properties-common build-essential && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y neovim

# TODO: Add extra dependencies, if needed
RUN git clone https://codeberg.org/ngn/k

# Commands for docker run
CMD cd k && \
    make && \
    cd ..
CMD make clean && \
    make ready

