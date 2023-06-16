FROM alpine:3.18.0 AS build

RUN apk add cmake g++ git samurai make

COPY . /src
WORKDIR /src

RUN mkdir -p build && make install

FROM alpine:3.18.0
RUN apk add libstdc++ rlwrap
COPY --from=build /src/dist /dist
WORKDIR /dist

CMD ["./bin/tests"]
