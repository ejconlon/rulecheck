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
	stack ghci --ghci-options "-ghci-script ../.ghci-manual" searchterm:lib

# Clean any junk left by latex
.PHONY: clean-docs
clean-docs:
	rm -f docs/*.{aux,bbl,blg,fdb_latexmk,fls,log,nav,out,pdf,snm,synctex.gz,toc}
	rm -rf docs/_minted-*

# Build the latex report
.PHONY: report
report:
	cd docs && latexmk -pdf -pdflatex="pdflatex -shell-escape -interaction=nonstopmode" -use-make ./rulecheck-report.tex

# Clean and build the latex report
# For some reason having these as make deps is racy. Just run them in sequence.
.PHONY: full-report
full-report:
	$(MAKE) clean-docs
	$(MAKE) report

# Need pygmentize to generate the report
.PHONY: report-deps
report-deps:
	brew install python@3.11 pygments
