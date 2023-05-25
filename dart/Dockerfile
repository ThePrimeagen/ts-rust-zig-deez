FROM dart:3.0.0 AS build

WORKDIR /app

COPY pubspec.* ./
RUN dart pub get

COPY . .

RUN dart pub get --offline

RUN dart test
RUN dart compile exe bin/dart_deez.dart -o bin/main

FROM scratch

COPY --from=build /runtime/ /
COPY --from=build /app/test /app/test
COPY --from=build /app/bin/main /app/bin/main

CMD ["/app/bin/main", "/app/test/example.monkey"]