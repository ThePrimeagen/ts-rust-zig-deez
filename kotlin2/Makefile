help:
	@cat Makefile | grep -E "^\w+$:"

ready: fmt lint test

docker-build:
	docker build --no-cache . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -v $(shell pwd):/deez -t deez_$(notdir $(shell pwd))

fmt:
	@echo "===> Formatting"
	./gradlew ktlintFormat

lint:
	@echo "===> Linting"
	./gradlew ktlintCheck

test:
	@echo "===> Testing"
	./gradlew cleanTest test
