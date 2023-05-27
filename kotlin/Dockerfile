FROM zenika/kotlin
WORKDIR /jmonkey
COPY . .
RUN kotlinc Lexer.kt -include-runtime -d LexerKt.jar
RUN kotlinc Tests.kt -include-runtime -cp "LexerKt.jar" -d TestsKt.jar

CMD ["java", "-cp", "\"LexerKt.jar;TestsKt.jar\"", "TestsKt"]