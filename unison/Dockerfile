# Latest Ubuntu image
FROM ubuntu:latest

# Make a working directory
# WORKDIR /Unison 

# Basic system-level dependencies
RUN apt-get update && \
    apt install -y software-properties-common git curl build-essential && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y neovim
# TODO: add yours

# Extra dependencies
# TODO: add yours
RUN mkdir unisonlanguage && \
    curl -L https://github.com/unisonweb/unison/releases/download/release%2FM4i/ucm-linux.tar.gz --output unisonlanguage/ucm.tar.gz && \
    tar -xzf unisonlanguage/ucm.tar.gz -C unisonlanguage

COPY . / 
RUN ./unisonlanguage/ucm
RUN ./unisonlanguage/ucm transcript Compile.md

# offically site showing how to compile files `https://www.unison-lang.org/learn/at-a-glance/`
# RUN ucm run.compiled test.uc
# RUN ucm run.compiled testFile.uc

# Commands for docker run
# CMD make fmt && \
#     make lint && \
#     make test

