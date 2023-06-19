# Latest Ubuntu image
FROM ubuntu:latest

# Make a working directory, as it would be binded with the current language one
WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && apt install -y make git curl libicu70 && \
    curl https://dotnet.microsoft.com/download/dotnet/scripts/v1/dotnet-install.sh -o install.sh && \
    chmod +x install.sh && ./install.sh -c 8.0

ENV DOTNET_ROOT=/root/.dotnet
ENV PATH=$PATH:${DOTNET_ROOT}:${DOTNET_ROOT}/tools

# Commands for docker run
CMD make clean && \
    make ready
