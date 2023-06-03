IMAGE=lexer-csharp2

all:
	@echo "Commands: bench, clean, fmt, test, ready, docker-build, docker-ready"

bench:
	@dotnet run -c release --project lexer.bench

clean:
	@dotnet clean lexer
	@dotnet clean lexer.tests
	@dotnet clean lexer.bench

fmt:
	@dotnet format lexer
	@dotnet format lexer.tests
	@dotnet format lexer.bench

test:
	@dotnet test lexer.tests

ready: fmt test

docker-build:
	@docker build -t ${IMAGE} .

docker-ready: docker-build
	@docker run --rm -ti --entrypoint "dotnet" ${IMAGE} test tests/lexer.tests.dll