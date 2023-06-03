# Use a Debian base image
FROM debian:buster

# Install necessary dependencies
RUN apt-get update && apt-get install -y \
    git \
    make \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Install V
RUN git clone https://github.com/vlang/v \
    && cd v \
    && make \
    && ln -s "$(pwd)/v" /usr/local/bin/v \
    && cd .. \
    && rm -rf v

# Set the working directory inside the container
WORKDIR /app

# Install V dependencies and build the application
RUN chmod +x run.sh

# run the lexer after tests
RUN ./run.sh

# Set the entry point
ENTRYPOINT ["v", "run", "lexer.v"]

# Tag the image
LABEL maintainer="Soumik Das mail2soumikdas@gmail.com"