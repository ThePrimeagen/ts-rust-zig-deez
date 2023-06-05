IMAGE=monkey-csharp2

help:
	@echo "Commands: bench, clean, fmt, test, ready, docker-build, docker-ready"

bench:
	@echo "==> Benchmarking"
	@dotnet run -c release --project MonkeyBench

clean:
	@echo "==> Cleaning"
	@dotnet clean Monkey
	@dotnet clean MonkeyTests
	@dotnet clean MonkeyBench

fmt:
	@echo "==> Formating and Linting"
	@dotnet format Monkey
	@dotnet format MonkeyTests
	@dotnet format MonkeyBench

test:
	@echo "==> Testing"
	@dotnet test MonkeyTests

ready: fmt test

docker-build:
	@docker build -t ${IMAGE} .

docker-ready: docker-build
	@docker run --rm -ti --entrypoint "dotnet" ${IMAGE} test tests/MonkeyTests.dll