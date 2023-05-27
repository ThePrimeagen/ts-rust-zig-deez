FROM clojure:latest

RUN mkdir /deez
WORKDIR /deez
COPY . .
# @fixme: this is somehow missing a few deps!
RUN clj -A:deps -A:cljfmt -A:test -A:lint
#RUN clj -X:deps prep

CMD make fmt && \
    make lint && \
    make test
