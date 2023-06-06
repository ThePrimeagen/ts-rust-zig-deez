# Latest Ubuntu image
FROM ubuntu:latest

# Make a working directory
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && \
    apt install -y software-properties-common git curl build-essential && \
    apt-get install -y wget libgmp-dev

# Extra dependencies
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_GHC_VERSION=9.4.5
ENV BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.1.0
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH=${PATH}:/root/.ghcup/bin

RUN wget https://github.com/fourmolu/fourmolu/releases/download/v0.12.0.0/fourmolu-0.12.0.0-linux-x86_64
RUN chmod +x fourmolu-0.12.0.0-linux-x86_64
RUN mv fourmolu-0.12.0.0-linux-x86_64 /usr/local/bin/fourmolu

RUN wget https://github.com/ndmitchell/hlint/releases/download/v3.5/hlint-3.5-x86_64-linux.tar.gz
RUN tar -xvf hlint-3.5-x86_64-linux.tar.gz
RUN mv hlint-3.5/hlint /usr/local/bin/hlint

# Commands for docker run
CMD make clean && \
    make ready ; \
    make clean
