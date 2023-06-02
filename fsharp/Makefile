##
# Static part, don't change these:
##

.PHONY: help ready docker-build docker-ready clean fmt lint test

help:
	@cat Makefile | grep -E "^\w+$:"

ready: fmt lint test

docker-build:
	docker build . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -v $(shell pwd):/deez -t deez_$(notdir $(shell pwd))

##
# Update those:
##

clean:
	@echo "===> Cleaning"
	dotnet restore
	dotnet clean --verbosity minimal --configuration Release

fmt:
	@echo "===> Formatting"
	fantomas Monkey/*.fs Monkey/*.fsi Monkey.Tests/*.fs

lint:
	@echo "===> Linting"
	dotnet fsharplint lint Monkey/Monkey.fsproj
	dotnet fsharplint lint Monkey.Tests/Tests.fs

test:
	@echo "===> Testing"
	dotnet test -c Release
