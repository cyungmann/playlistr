SRCS := $(shell find src/ -type f -name '*.hs')
OUT := dist/build/playlistr/playlistr
.DEFAULT_GOAL := all

.PHONY: all
all: $(OUT)

$(OUT): $(SRCS)
	cabal --require-sandbox build

define format-command =
cabal --require-sandbox exec hindent -- $(1)
cabal --require-sandbox exec stylish-haskell -- --inplace $(1)
endef

.format-stamp: $(SRCS)
	$(foreach src,$(SRCS),$(call format-command,$(src)))
	touch .format-stamp

.PHONY: format
format: .format-stamp

FORCE:

define lint-command =
cabal --require-sandbox exec hlint -- lint $(1)
endef

.PHONY: lint
lint: FORCE
	$(foreach src,$(SRCS),$(call lint-command,$(src)))

.PHONY: run
run: FORCE
	cabal --require-sandbox run -- ./.private/config.yaml

.PHONY: repl
repl: FORCE
	cabal --require-sandbox repl --ghc-options="-Wwarn"

.PHONY: watch
watch: FORCE
	cabal --require-sandbox exec ghcid -- --command='cabal repl --ghc-options="-Wwarn"'

.PHONY: clean
clean: FORCE
	cabal --require-sandbox clean
	rm .*-stamp

.PHONY: devtools
devtools: FORCE
	cabal --require-sandbox install --force-reinstalls ghcid hlint stylish-haskell hindent
