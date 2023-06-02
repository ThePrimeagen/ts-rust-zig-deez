FROM nimlang/nim:latest as builder

WORKDIR /app

COPY src/ src/
COPY tests/ tests/
COPY lexer.nimble .

ENTRYPOINT ["nimble", "test", "-d:release"]
