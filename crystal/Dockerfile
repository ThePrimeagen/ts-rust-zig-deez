FROM ubuntu:latest

RUN apt-get install -y crystal
COPY . /app
WORKDIR /app

RUN crystal spec
RUN shards build

ENTRYPOINT [ "/app/bin/deez" ]
