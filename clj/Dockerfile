FROM clojure:lein-2.9.1-alpine

RUN apk add --update-cache curl bash git

# install babashka
RUN curl -sLO https://raw.githubusercontent.com/babashka/babashka/master/install \
    && chmod +x install \
    && ./install

WORKDIR /app

COPY project.clj .

RUN bb prepare

COPY . .

CMD ["bb", "test"]