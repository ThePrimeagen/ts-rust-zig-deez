FROM ubuntu:latest

WORKDIR /deez

RUN apt-get update && \
    apt-get install -y \
      build-essentials \
      portaudio19-dev libsdl2-dev yasm \
      git

RUN git clone --depth 1 https://git.xslendi.xyz/nrootconauto/3Days.git /deez/3days
RUN cd 3days && make
RUN cd /deez/lexer && /deez/3days/3d_loader -t /deez/3days/T -c Tests.HC | grep -q 'Tests done'

