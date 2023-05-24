FROM alpine:3.18.0

RUN apk add cmake make g++ git

COPY . /src

RUN mkdir /src/cbuild

WORKDIR /src/cbuild

RUN cmake ../ && make

CMD ["./tests/lexer_test"]
