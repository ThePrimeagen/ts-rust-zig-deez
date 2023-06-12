# Latest Ubuntu image
FROM ubuntu:latest

# Make a working directory
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && \
    apt install -y software-properties-common git curl build-essential gcc && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y neovim && \
    apt-get install -y unzip

# Extra dependencies
RUN wget -O linenoise.zip https://github.com/antirez/linenoise/archive/refs/heads/master.zip && \
    unzip linenoise.zip && \
    cd linenoise-master && \
    gcc -c -Os -g -o linenoise.o linenoise.c && \
    ar rcs liblinenoise.a linenoise.o && \
    cp liblinenoise.a /usr/local/lib/

RUN curl -fsS https://dlang.org/install.sh | bash -s dmd
RUN apt-get install -y dub

dub run dfmt -- -h
dub run dscanner -- -h

# Commands for docker run
CMD make fmt && \
    make lint && \
    make test

