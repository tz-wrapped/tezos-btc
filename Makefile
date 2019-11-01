# SPDX-FileCopyrightText: 2019 Serokell <https://serokell.io>, 2019 Bitcoin Suisse
#
# SPDX-License-Identifier: MPL-2.0
#
.PHONY: tzbtc test test-ci haddock haddock-no-deps stylish lint init-doc upload-doc clean all

# Build target from the common utility Makefile
MAKEU = $(MAKE) -C make/

MAKE_PACKAGE = $(MAKEU) PACKAGE=tzbtc

tzbtc:
	$(MAKE_PACKAGE) dev
test:
	$(MAKE_PACKAGE) test
test-ci:
	$(MAKE_PACKAGE) test-ci
test-dumb-term:
	$(MAKE_PACKAGE) test-dumb-term
test-hide-successes:
	$(MAKE_PACKAGE) test-hide-successes
haddock:
	$(MAKE_PACKAGE) haddock
haddock-no-deps:
	$(MAKE_PACKAGE) haddock-no-deps
stylish:
	$(MAKE_PACKAGE) stylish
lint:
	$(MAKE_PACKAGE) lint
init-doc:
	./scripts/init-autodoc.sh
upload-doc:
	./scripts/upload-autodoc.sh $(GIST_ID)
clean:
	$(MAKE_PACKAGE) clean
all:
	$(MAKEU) PACKAGE=""
test-all:
	$(MAKEU) test PACKAGE=""
