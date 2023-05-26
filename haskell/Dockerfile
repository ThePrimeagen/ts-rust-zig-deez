FROM ubuntu:latest

# Make a working directory
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && \
    apt install -y software-properties-common git curl wget build-essential libgmp3-dev && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y neovim

# Install Haskek
WORKDIR /opt/haskell

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

RUN mkdir -p /root/.local/bin
ENV PATH=${PATH}:/root/.local/bin
ENV PATH=${PATH}:/root/.ghcup/bin

RUN ghcup upgrade
RUN ghcup install cabal 3.10.1.0
RUN ghcup set cabal 3.10.1.0
RUN ghcup install ghc 9.4.4
RUN ghcup set ghc 9.4.4

RUN wget https://github.com/fourmolu/fourmolu/releases/download/v0.12.0.0/fourmolu-0.12.0.0-linux-x86_64
RUN chmod +x fourmolu-0.12.0.0-linux-x86_64
RUN mv fourmolu-0.12.0.0-linux-x86_64 /root/.local/bin/fourmolu

RUN wget https://github.com/ndmitchell/hlint/releases/download/v3.5/hlint-3.5-x86_64-linux.tar.gz
RUN tar -xvf hlint-3.5-x86_64-linux.tar.gz
RUN mv hlint-3.5/hlint /root/.local/bin/hlint

RUN cabal update

# Build the project
WORKDIR /deez

COPY ./haskell.cabal ./haskell.cabal

RUN cabal build --only-dependencies --enable-tests -j4

COPY . .

RUN cabal build

# Commands for docker run
CMD make fmt && \
    make lint && \
    make test
