FROM frolvlad/alpine-fpc:latest

RUN mkdir /src

COPY ./src /src

RUN fpc /src/Lexer.lpr
RUN fpc /src/Lexer.Tests.lpr

CMD ["/bin/sh", "-c", "/src/Lexer.Tests -a"]

