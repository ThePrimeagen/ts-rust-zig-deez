FROM swift:latest

WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && apt install -y make git curl && \
    # TODO: these are examplary, change to those you need:
    apt install -y software-properties-common build-essential && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y neovim

# TODO: Add extra dependencies, if needed

COPY . .

# Commands for docker run
CMD make clean && \
    make ready