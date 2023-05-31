# Latest Ubuntu image
FROM ubuntu:latest

# Make a working directory
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && \
    apt install -y software-properties-common git curl make && \
    apt install -y shellcheck

# Commands for docker run
CMD make fmt && \
    make lint && \
    make test

