# Latest Haskell Image
FROM haskell:9.4.5

# Make a working directory
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && \
    apt install -y software-properties-common git curl wget build-essential && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y neovim

# Install Haskek Tools
WORKDIR /opt/haskell

RUN mkdir -p /root/.local/bin
ENV PATH=${PATH}:/root/.local/bin

RUN wget https://github.com/ndmitchell/hlint/releases/download/v3.5/hlint-3.5-x86_64-linux.tar.gz
RUN tar -xvf hlint-3.5-x86_64-linux.tar.gz
RUN mv hlint-3.5/hlint /root/.local/bin/hlint

# Build the project
WORKDIR /deez

RUN cabal update

COPY haskell.cabal haskell.cabal

RUN cabal build --only-dependencies --enable-tests -j4

COPY . .

RUN cabal build

# Commands for docker run
CMD make fmt && \
    make lint && \
    make test
