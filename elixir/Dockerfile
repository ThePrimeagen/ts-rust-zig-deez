# Latest Ubuntu image
FROM elixir:1.14-otp-25-slim

# Make a working directory
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && \
    apt install -y software-properties-common git curl build-essential && \
    add-apt-repository --yes ppa:neovim-ppa/unstable && \
    apt-get install -y neovim

# Commands for docker run
CMD mix deps.get && \
    mix test --warnings-as-errors
