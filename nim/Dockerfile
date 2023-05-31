FROM nimlang/nim as builder

WORKDIR /app


COPY . /app

RUN nimble build -d:release lexer.nim

FROM alpine:3

COPY --from=builder /app/bin/lexer /lexer

ENTRYPOINT [ "/lexer" ]