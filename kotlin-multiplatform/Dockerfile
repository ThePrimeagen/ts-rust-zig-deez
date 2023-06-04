FROM gradle:8.1.1-jdk11-alpine

COPY --chown=gradle:gradle . /app
WORKDIR /app

CMD echo "===> Formatting"
CMD ./gradlew formatKotlin

CMD echo "===> Linting"
CMD ./gradlew lintKotlin

CMD echo "===> Testing"
CMD ./gradlew cleanAllTests allTests
