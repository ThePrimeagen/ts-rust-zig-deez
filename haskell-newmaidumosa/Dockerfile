# Latest Ubuntu image
FROM ubuntu:latest

# Make a working directory, as it would be binded with the current language one
WORKDIR /deez

# Mandatory system-level dependencies
RUN apt-get update && apt install -y make git curl

# TODO: Add extra dependencies, if needed. Here's an example:
# RUN apt install -y software-properties-common build-essential && \
#     add-apt-repository --yes ppa:neovim-ppa/unstable && \
#     apt-get install -y neovim

# Commands for docker run. They should not be running forever.
# If you need such behavior, just add another file, say: repl.Dockerfile
CMD make clean && \
    make ready

