FROM ubuntu:latest

WORKDIR /deez

COPY lexer /deez/lexer

ENV threedays="/deez/3days"

RUN apt-get update && \
    apt-get install -y \
      build-essential yasm \
      git

RUN git clone --depth 1 https://git.xslendi.xyz/slendi/3Days.git /deez/3days
RUN cd 3days && make
RUN mv /deez/lexer/* /deez/3days/.

CMD cd /deez/3days && ./3d_loader -t /deez/3days/T -c TestsExit.HC

