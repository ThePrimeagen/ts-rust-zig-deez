##
# Static part, don't change these:
##

.PHONY: help ready docker-build docker-ready fmt lint test

help:
	@cat Makefile | grep -E "^\w+$:"

ready: fmt lint test

docker-build:
	@echo "\n===> Building docker image"
	@docker build --no-cache . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	@echo "\n===> Running docker image"
	@docker run -v $(shell pwd):/deez -t deez_$(notdir $(shell pwd))

##
# Update these:
##

clean:
	@echo "\n===> Cleaning"
	@rm -f *.log && echo "cleaned up the logs 🪵"

fmt:
	@echo "\n===> Formatting"
	@(npm-groovy-lint --fix --output txt --files "**/*.groovy" > fmt.log 2>&1 --failon none && echo "✨ made it look nicer ✨") || (echo "🤢 your code is so ugly it couldn't even be formatted 🤢" && cat fmt.log)

lint:
	@echo "\n===> Linting"
	@(npm-groovy-lint --files "**/*.groovy" --output txt --failon warning > lint.log 2>&1 && \
		echo "heyyy the lint checks passed... this time 😉") || \
			(echo "your code uh.... didn't pass the lint checks 😑" && cat lint.log)

test:
	@echo "\n===> Testing EVERYTHING"
	@(groovy Test.groovy 2>/dev/null && \
		echo "😎 every test passed you absolute legend 😎") || \
			echo "❌❌❌ NO! NO! SOME OF THE TESTS FAILED! ❌❌❌"

test-lexer:
	@echo "\n===> Testing lexer"
	@(groovy lexer/LexerTest.groovy 2>/dev/null && \
		echo "nice! 🎉 the lexer tests passed") || \
			echo "wow. the 🤬 lexer tests failed... you good?"

test-ast:
	@echo "\n===> Testing AST"
	@echo "CUMMMING SOONNNNN"

test-parser:
	@echo "\n===> Testing parser"
	@echo "CUMMMING SOONNNNN"
