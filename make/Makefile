# SPDX-FileCopyrightText: 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: LicenseRef-Proprietary
#
# Defines utilities for other Makefiles

.PHONY: dev test haddock haddock-no-deps stylish lint clean

# Options for development
STACK_DEV_OPTIONS = --fast --ghc-options -Wwarn --file-watch
# Options to build more stuff (tests and benchmarks)
STACK_BUILD_MORE_OPTIONS = --test --bench --no-run-tests --no-run-benchmarks
# Options for tests
STACK_DEV_TEST_OPTIONS = --fast
# Addtional (specified by user) options passed to test executable
TEST_ARGUMENTS ?= ""
# Packages to apply the command (build, test, e.t.c) for.
PACKAGE ?= non-defined-package

define call_test
	stack test $(PACKAGE) $(STACK_DEV_TEST_OPTIONS) \
		--test-arguments "--color always $(TEST_ARGUMENTS) $1"
endef

# Build everything (including tests and benchmarks) with development options.
dev:
	stack build $(STACK_DEV_OPTIONS) $(STACK_BUILD_MORE_OPTIONS) $(PACKAGE)

# Run tests in all packages which have them.
test:
	$(call call_test,"")

# Like 'test' command, but enforces dumb terminal which may be useful to
# workardoung some issues with `tasty`.
# Primarily this one: https://github.com/feuerbach/tasty/issues/152
test-dumb-term:
	TERM=dumb $(call call_test,"")

# Run tests with `--hide-successes` option. It forces dumb terminal,
# because otherwise this option is likely to work incorrectly.
test-hide-successes:
	TERM=dumb $(call call_test,"--hide-successes")

# Run haddock for all packages.
haddock:
	stack haddock $(STACK_DEV_OPTIONS) $(PACKAGE)

# Run haddock for all our packages, but not for dependencies.
haddock-no-deps:
	stack haddock $(STACK_DEV_OPTIONS) $(PACKAGE) --no-haddock-deps

stylish:
	stylish-haskell -i `find src -iname '*.hs'`

lint:
	cd .. && scripts/lint.sh

clean:
	stack clean $(PACKAGE)
