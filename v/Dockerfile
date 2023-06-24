FROM fedora:38

WORKDIR /deez

COPY . /deez
ADD . .

RUN dnf -y update && dnf -y install make gcc git curl && \
    dnf -y install neovim python3-neovim && \
    dnf -y install libatomic

RUN cd ~ && \
    git clone https://github.com/vlang/v && \
    cd v && \
    make && \
    ./v symlink && \
    v up

CMD make clean && \
    make ready
