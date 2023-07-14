# Latest Ubuntu image
FROM ubuntu:latest

# Make a working directory
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && \
    apt install -y software-properties-common git curl build-essential gcc && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y wget && \
    apt-get install -y neovim && \
    apt-get install -y unzip

# Install D packages
RUN curl -fsS https://dlang.org/install.sh | bash -s dmd
RUN apt-get install -y dub

RUN wget https://master.dl.sourceforge.net/project/d-apt/files/d-apt.list -O /etc/apt/sources.list.d/d-apt.list
RUN apt update --allow-insecure-repositories && apt -y --allow-unauthenticated install --reinstall d-apt-keyring && apt update

RUN apt-get install -y dfmt && \
    apt-get install -y dscanner

# Commands for docker run and
# extra dependencies
CMD wget -O linenoise.zip https://github.com/antirez/linenoise/archive/refs/heads/master.zip && \
    unzip linenoise.zip && \
    cd linenoise-master && \
    gcc -c -Os -g -o linenoise.o linenoise.c && \
    ar rcs liblinenoise.a linenoise.o && \
    mkdir -p ../lib && \
    cp liblinenoise.a ../lib && \
    cd .. && \
    rm -rf linenoise-master && \
    make ready

