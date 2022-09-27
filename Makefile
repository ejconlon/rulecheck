include Makefile.base

.PHONY: exe
exe:
	stack build --test --no-run-tests --exec rulecheck-exe
