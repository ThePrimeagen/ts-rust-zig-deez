# Latest Haskell Image
FROM haskell:9.4.5

# Make a working directory
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && \
    apt install -y software-properties-common git curl build-essential && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y neovim

# Build the project
RUN cabal update

COPY . .

RUN cabal build --enable-tests -j4

# Commands for docker run
CMD cabal test
