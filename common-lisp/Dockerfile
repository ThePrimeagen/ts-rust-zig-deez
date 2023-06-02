# Latest Ubuntu image
FROM ubuntu:latest

# Make a working directory
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && \
    apt install -y software-properties-common git curl build-essential && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y neovim

RUN apt install -y sbcl

# Install quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp

RUN sbcl --load quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
    --eval '(uiop:quit)'

# Commands for docker run
CMD make fmt && \
    make lint && \
    make test
