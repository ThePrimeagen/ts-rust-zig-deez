FROM mcr.microsoft.com/dotnet/runtime:7.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:7.0 AS build
WORKDIR /src
COPY ["Monkey/Monkey.fsproj", "Monkey/"]
RUN dotnet restore "Monkey/Monkey.fsproj"
COPY . .
WORKDIR "/src/Monkey"
RUN dotnet build "Monkey.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "Monkey.fsproj" -c Release -o /app/publish /p:UseAppHost=false

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "Monkey.dll"]
