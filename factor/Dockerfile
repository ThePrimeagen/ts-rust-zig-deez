# TODO: FINISH THIS

FROM ubuntu:latest

# Make a working directory, as it would be binded with the current language one
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && apt install -y make git curl && \
    # TODO: these are examplary, change to those you need:
    apt install -y software-properties-common build-essential && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y neovim


RUN wget https://downloads.factorcode.org/linux-x86-64/factor-linux-x86-64-2023-05-31-14-38.tar.gz && \
    tar xvf linux-x86-64/factor-linux-x86-64-2023-05-31-14-38.tar.gz && \


# TODO: Add extra dependencies, if needed

# Commands for docker run
CMD make clean && \
    make ready