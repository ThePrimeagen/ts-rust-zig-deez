# Latest Ubuntu image
FROM ubuntu:latest

# Make a working directory, as it would be binded with the current language one
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && apt install -y make git curl

# nelua dependencies
RUN apt install -y build-essential gcc gdb

# nelua installation
RUN git clone https://github.com/edubart/nelua-lang.git && \
    cd nelua-lang && \
    make && make install

RUN rm -rf /deez/nelua-lang

# Commands for docker run
CMD make clean && \
    make ready
