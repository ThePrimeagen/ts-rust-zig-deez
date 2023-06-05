# Latest Ubuntu image
FROM ubuntu:latest

RUN mkdir deez
COPY . /deez
# Make a working directory, as it would be binded with the current language one
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && apt install -y make git curl && \
    # TODO: these are examplary, change to those you need:
    apt-get install -y openjdk-8-jdk && \
    apt-get install -y ant && \
    apt-get clean

# Commands for docker run
CMD make clean && \
    make ready &&
