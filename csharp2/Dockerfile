FROM mcr.microsoft.com/dotnet/sdk:7.0-alpine AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:7.0-alpine AS restore
WORKDIR /src
COPY ["Monkey/Monkey.csproj", "Monkey/"]
COPY ["MonkeyTests/MonkeyTests.csproj", "MonkeyTests/"]
RUN dotnet restore --ucr "Monkey/Monkey.csproj" && \
    dotnet restore --ucr "MonkeyTests/MonkeyTests.csproj"

FROM restore AS publish
COPY . .
RUN dotnet publish -c release --no-restore "Monkey/Monkey.csproj" \
    -p:publishtrimmed=true --ucr -o /app/publish && \
    dotnet publish -c release --no-restore "MonkeyTests/MonkeyTests.csproj" \
    --ucr -o /app/tests

FROM base as final
COPY --from=publish /app/publish ./app
COPY --from=publish /app/tests ./tests
ENTRYPOINT ["./app/Monkey"]
