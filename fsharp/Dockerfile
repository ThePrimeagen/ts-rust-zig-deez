FROM mcr.microsoft.com/dotnet/sdk:7.0-jammy

WORKDIR /deez

# Basic system-level dependencies
RUN apt-get update && apt install -y make git curl

ENV PATH="${PATH}:/root/.dotnet/tools"

# Formatter and linter
RUN dotnet tool install -g fantomas && \
    dotnet tool install -g dotnet-fsharplint

# Commands for docker run
CMD make clean && \
    make ready
