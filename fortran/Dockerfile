FROM ubuntu:22.04
RUN apt update
RUN apt install -y make gfortran

COPY . /app
WORKDIR /app

RUN make test
RUN make repl

CMD ["./repl"]
