# latest official Groovy image
FROM groovy:latest

# make a working directory
WORKDIR /deez

# copy the source files into the container
COPY . .

# dependencies
USER root
RUN apt-get update && apt-get install -y make git curl && \
    curl -sL https://deb.nodesource.com/setup_20.x | bash - && \
    apt-get install -y nodejs && \
    npm install -g npm-groovy-lint

# commands for docker run
CMD make clean && \
    make ready
