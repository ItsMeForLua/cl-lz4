# Makefile for cl-lz4

# Default to 'ros run', which uses the user's Roswell default.
# Can be overridden, e.g., 'make LISP=sbcl' or 'make LISP="ros -L ccl-bin run"'
LISP ?= ros run
QLOT ?= $(shell which qlot || if [ -f "$(HOME)/.roswell/bin/qlot" ]; then echo "$(HOME)/.roswell/bin/qlot"; else echo ""; fi)

.PHONY: all deps clean test test-local benchmark benchmark-local help

all: help

# Install Lisp dependencies using qlot (requires network)
deps:
	@if [ -z "$(QLOT)" ]; then \
		echo "[ERROR] qlot not found. Please install it (e.g., 'ros install qlot')."; \
		exit 1; \
	fi
	@echo "--> Installing Lisp dependencies with qlot..."
	@$(QLOT) install

# Clean build artifacts (for a pure lisp project, this is mainly the qlot cache)
clean:
	@echo "--> Cleaning qlot cache..."
	@rm -rf .qlot/
	@echo "Clean complete"

# Run tests with qlot (Requires network for deps)
test: deps
	@echo "--> Running tests with qlot..."
	@$(QLOT) exec $(LISP) --non-interactive \
		--eval '(push *default-pathname-defaults* asdf:*central-registry*)' \
		--eval "(asdf:test-system :cl-lz4-tests)"

# Run tests with system Lisp (Works offline)
test-local:
	@echo "--> Running tests with local Roswell environment (no qlot)..."
	@$(LISP) --non-interactive \
		--eval '(push *default-pathname-defaults* asdf:*central-registry*)' \
		--eval "(ql:quickload :fiveam)" \
		--eval "(asdf:test-system :cl-lz4-tests)"

# Run benchmarks with qlot (Requires network for deps)
benchmark: deps
	@echo "--> Running benchmarks with qlot..."
	@$(QLOT) exec $(LISP) --non-interactive \
		--eval '(push *default-pathname-defaults* asdf:*central-registry*)' \
		--eval "(ql:quickload :cl-lz4-benchmarks)" \
		--eval "(cl-lz4-benchmarks:run-benchmarks)" \
		--eval "(uiop:quit)"

# Run benchmarks with system Lisp (Works offline)
benchmark-local:
	@echo "--> Running benchmarks with local Roswell environment (no qlot)..."
	@$(LISP) --non-interactive \
		--eval '(push *default-pathname-defaults* asdf:*central-registry*)' \
		--eval "(ql:quickload :cl-lz4-benchmarks)" \
		--eval "(cl-lz4-benchmarks:run-benchmarks)" \
		--eval "(uiop:quit)"

help:
	@echo "cl-lz4 Build System"
	@echo ""
	@echo "== Common Commands =="
	@echo "  make test             - Run tests in a reproducible qlot environment (needs network)."
	@echo "  make test-local       - Run tests in your local Roswell environment (works offline)."
	@echo "  make benchmark        - Run benchmarks in a reproducible qlot environment (needs network)."
	@echo "  make benchmark-local  - Run benchmarks in your local Roswell environment (works offline)."
	@echo "  make deps             - Install Lisp dependencies using qlot."
	@echo "  make clean            - Clean all build artifacts."

