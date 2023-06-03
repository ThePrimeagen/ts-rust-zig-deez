# syntax=docker/dockerfile:1.2
ARG JAVA_VERSION=20

FROM maven:3-eclipse-temurin-${JAVA_VERSION}-alpine AS build
WORKDIR /jmonkey
COPY . .
RUN --mount=type=cache,target=/root/.m2 mvn -e -f ./monkey/pom.xml clean test
RUN --mount=type=cache,target=/root/.m2 mvn -e -f ./monkey/pom.xml clean package
RUN mv monkey/target/monkey-1.0-SNAPSHOT.jar ./monkey.jar

FROM eclipse-temurin:${JAVA_VERSION}-alpine
WORKDIR /jmonkey
COPY --from=build /jmonkey/monkey.jar .
CMD ["java","--enable-preview","-jar", "monkey.jar"]
