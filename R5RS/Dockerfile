FROM ubuntu:latest

RUN apt-get update -y && apt install racket -y

COPY . /deez
WORKDIR /deez

CMD cd tests && plt-r5rs test.rkt