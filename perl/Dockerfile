FROM ubuntu:latest

COPY . /src

WORKDIR /src

RUN apt-get update && apt install -y make perl

CMD make clean && \
    make ready
