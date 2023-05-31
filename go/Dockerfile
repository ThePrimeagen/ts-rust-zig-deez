# Latest Ubuntu image
FROM ubuntu:latest

# Make a working directory, as it would be binded with the current language one
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && apt install -y make git curl && \
    # TODO: these are examplary, change to those you need:
    apt install -y golang-go

# TODO: Add extra dependencies, if needed

COPY . .

# Commands for docker run
CMD ["go", "test", "-v"]
