# Disable default make target
.PHONY: default
default:
	echo "No default target"

# Create the virtual environment
.PHONY: venv
venv:
	python3 -m venv --upgrade-deps .venv
	.venv/bin/python3 -m pip install wheel -r dev-requirements.txt -r requirements.txt

# Format - for now just sort imports
.PHONY: format
format:
	.venv/bin/python3 -m isort --settings-path=setup.cfg auto

# Typecheck with mypy
.PHONY: typecheck
typecheck:
	.venv/bin/python3 -m mypy -p auto

# Lint with flake8
.PHONY: lint
lint:
	.venv/bin/python3 -m flake8 --config=setup.cfg auto

# Unit test with pytest
.PHONY: unit
unit:
	if [ -d tests ]; then .venv/bin/python3 -m pytest tests; fi

# Run all tests
.PHONY: test
test: lint typecheck unit

# Clean most generated files (+ venv)
.PHONY: clean
clean:
	rm -rf .venv .mypy_cache .pytest_cache *.egg-info
