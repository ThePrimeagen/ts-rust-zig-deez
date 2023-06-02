FROM eclipse-temurin:17-jdk-alpine
WORKDIR /jmonkey
COPY . .
RUN mvn -f ./monkey/pom.xml test
CMD ["java", "Tests"]
