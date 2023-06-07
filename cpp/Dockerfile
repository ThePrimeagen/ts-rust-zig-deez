FROM gcc:latest
RUN apt-get update
RUN apt-get install -y cmake make
COPY . /src
RUN cmake -S /src -B /build -D CMAKE_BUILD_TYPE=Release
RUN cmake --build /build
CMD ["/build/lexer"]
