FROM alpine:latest
RUN apk add --update crystal shards yaml yaml-dev musl-dev pcre2 gc libevent libgcc
RUN mkdir /src
WORKDIR /src
RUN crystal spec
ENTRYPOINT [ "/usr/bin/deez" ]
