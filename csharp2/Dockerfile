FROM mcr.microsoft.com/dotnet/sdk:7.0-alpine AS base
WORKDIR /app
EXPOSE 80

FROM mcr.microsoft.com/dotnet/sdk:7.0-alpine AS restore
WORKDIR /src
COPY ["lexer/lexer.csproj", "lexer/"]
COPY ["lexer.tests/lexer.tests.csproj", "lexer.tests/"]
RUN dotnet restore --ucr "lexer/lexer.csproj" && \
    dotnet restore --ucr "lexer.tests/lexer.tests.csproj"

FROM restore AS publish
COPY . .
RUN dotnet publish -c release --no-restore "lexer/lexer.csproj" \
    -p:publishtrimmed=true --ucr -o /app/publish && \
    dotnet publish -c release --no-restore "lexer.tests/lexer.tests.csproj" \
    --ucr -o /app/tests

FROM base as final
COPY --from=publish /app/publish ./lexer
COPY --from=publish /app/tests ./tests
ENTRYPOINT ["./lexer/lexer"]
