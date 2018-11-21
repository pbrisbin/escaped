all: setup setup.lint build test lint

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)
	stack build $(STACK_ARGUMENTS) \
	  --dependencies-only --test --no-run-tests

.PHONY: setup.lint
setup.lint:
	stack install $(STACK_ARGUMENTS) \
	  --copy-compiler-tool hlint weeder

.PHONY: setup.coverage
setup.coverage:
	stack install $(STACK_ARGUMENTS) \
	  --copy-compiler-tool cc-coverage
	curl -L -O cc-test-reporter \
	  https://codeclimate.com/downloads/test-reporter/test-reporter-latest-linux-amd64
	chmod +x ./cc-test-reporter

.PHONY: build
build:
	stack build $(STACK_ARGUMENTS) \
	  --coverage \
	  --fast --pedantic --test --no-run-tests

.PHONY: watch
watch:
	stack build $(STACK_ARGUMENTS) \
	  --coverage \
	  --fast --pedantic --test --file-watch

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) \
	  --coverage \
	  --fast --pedantic --test

.PHONY: lint
lint:
	stack exec hlint .
	stack exec weeder .

.PHONY: coverage
coverage:
	stack exec tix2cc | ./cc-test-reporter upload-coverage --input -

.PHONY: clean
clean:
	stack clean
