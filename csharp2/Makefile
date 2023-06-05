IMAGE=monkey-csharp2

all:
	@echo "Commands: bench, clean, fmt, test, ready, docker-build, docker-ready"

bench:
	@dotnet run -c release --project MonkeyBench

clean:
	@dotnet clean Monkey
	@dotnet clean MonkeyTests
	@dotnet clean MonkeyBench

fmt:
	@dotnet format Monkey
	@dotnet format MonkeyTests
	@dotnet format MonkeyBench

test:
	@dotnet test MonkeyTests

ready: fmt test

docker-build:
	@docker build -t ${IMAGE} .

docker-ready: docker-build
	@docker run --rm -ti --entrypoint "dotnet" ${IMAGE} test tests/MonkeyTests.dll