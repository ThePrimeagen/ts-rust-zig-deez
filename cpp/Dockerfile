FROM alpine:3.18.0

RUN apk add cmake g++ git samurai

COPY . /src
WORKDIR /src

RUN mkdir -p build
RUN cmake -S . -B build -G Ninja
RUN cmake --build build

CMD ["./build/tests/tests"]
