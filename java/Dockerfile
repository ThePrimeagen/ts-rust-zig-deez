# syntax=docker/dockerfile:1.2
ARG JAVA_VERSION=21

FROM maven:3-eclipse-temurin-${JAVA_VERSION}-alpine AS build
WORKDIR /jmonkey
COPY . .
RUN mvn -e -f ./monkey/pom.xml clean test
RUN mvn -e -f ./monkey/pom.xml clean package
RUN mv monkey/target/monkey-1.0-SNAPSHOT.jar ./monkey.jar

FROM eclipse-temurin:${JAVA_VERSION}-alpine
WORKDIR /jmonkey
COPY --from=build /jmonkey/monkey.jar .
ENTRYPOINT ["java", "-jar", "monkey.jar"]
CMD ["let fac = fn (n) { let it = fn (n, acc) { if (n < 1) { acc } else { it(n-1, n*acc) }}; it(n, 1) }; fac(pow(10))"]
