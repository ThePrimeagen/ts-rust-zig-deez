FROM dart:stable AS build

WORKDIR /app

COPY pubspec.* ./
RUN dart pub get

COPY . .
COPY bin/ bin/ 
COPY bin/ /app/bin/ 
COPY bin/main.dart /app/bin/main.dart 

RUN dart pub get --offline

RUN dart test
RUN dart compile exe bin/main.dart -o bin/main

FROM scratch

COPY --from=build /runtime/ /
COPY --from=build /app/test /app/test
COPY --from=build /app/bin/main /app/bin/main

CMD ["/app/bin/main", "/app/test/example.monkey"]