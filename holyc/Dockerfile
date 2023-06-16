FROM ubuntu:latest

WORKDIR /deez
RUN mkdir /deez/lexer

ENV threedays="/deez/3days"

RUN apt-get update
RUN apt-get install -y \
      build-essential yasm \
      git

RUN git clone --depth 1 https://git.xslendi.xyz/slendi/3Days.git /deez/3days

COPY Run.HC /deez/3days/Run.HC

RUN cd 3days && make -j $(nproc)

CMD cd /deez/3days && ./3d_loader -t /deez/3days/T -c Run.HC

