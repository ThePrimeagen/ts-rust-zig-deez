FROM ubuntu:22.04
RUN apt-get update -y
RUN apt-get install -y gfortran

COPY . /app
WORKDIR /app

RUN gfortran token.f90 lexer.f90 repl.f90 -o repl

CMD ["./repl"]
