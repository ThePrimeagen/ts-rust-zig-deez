FROM dart:stable AS build

WORKDIR /app

COPY pubspec.* ./
RUN dart pub get

COPY . .

RUN dart pub get --offline
# RUN dart test
RUN dart compile exe binny/main.dart -o binny/main

FROM scratch

COPY --from=build /runtime/ /
COPY --from=build /app/test /app/test
COPY --from=build /app/binny/main /app/bin/main

CMD ["/app/bin/main", "/app/test/example.monkey"]