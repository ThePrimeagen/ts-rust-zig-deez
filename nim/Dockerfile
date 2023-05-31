FROM nimlang/nim as builder

WORKDIR /app

COPY lexer.nim .

RUN nimble build -d:release lexer.nim

FROM alpine:3

COPY --from=builder /app/lexer /lexer

ENTRYPOINT [ "/lexer" ]