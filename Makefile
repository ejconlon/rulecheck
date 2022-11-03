include Makefile.base

# Because generated code may be in a bad state, we separate normal and generated targets
# Run normal ones without a suffix: build test, generateed ones as: build-gen test-gen
# and everything with: build-all test-all
TARGETS = demo-domain:test:demo-domain-test rulecheck:test:rulecheck-test searchterm:test:searchterm-test

.PHONY: exe
exe:
	stack build --fast --test --no-run-tests --exec rulecheck-exe

.PHONY: build
build:
	stack build --fast --test --no-run-tests $(TARGETS)

.PHONY: test
test:
	stack build --fast --test $(TARGETS)

.PHONY: build-gen
build-gen:
	stack build --fast --test --no-run-tests demo-test:test:demo-test

.PHONY: test-gen
test-gen:
	stack build --fast --test demo-test:test:demo-test

.PHONY: build-all
build-all:
	stack build --fast --test --no-run-tests

.PHONY: test-all
test-all:
	stack build --fast --test

.PHONY: test-searchterm
test-searchterm:
	stack build --fast --test searchterm:test:searchterm-test

.PHONY: ghci-searchterm
ghci-searchterm:
	stack ghci --ghci-options "-ghci-script ../.ghci-manual" searchterm:test:searchterm-test
