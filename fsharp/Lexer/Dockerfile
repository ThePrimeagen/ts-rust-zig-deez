FROM mcr.microsoft.com/dotnet/runtime:7.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:7.0 AS build
WORKDIR /src
COPY ["Lexer/Lexer.fsproj", "Lexer/"]
RUN dotnet restore "Lexer/Lexer.fsproj"
COPY . .
WORKDIR "/src/Lexer"
RUN dotnet build "Lexer.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "Lexer.fsproj" -c Release -o /app/publish /p:UseAppHost=false

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "Lexer.dll"]
