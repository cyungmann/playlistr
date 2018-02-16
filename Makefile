SRCS := $(shell find src/ -type f -name '*.hs')
OUT := dist/build/playlistr/playlistr
.DEFAULT_GOAL := all

.PHONY: all
all: $(OUT)

$(OUT): $(SRCS)
	cabal build

define format-command =
cabal exec hindent -- $(1)
cabal exec stylish-haskell -- --inplace $(1)
endef

.format-stamp: $(SRCS)
	$(foreach src,$(SRCS),$(call format-command,$(src)))
	touch .format-stamp

.PHONY: format
format: .format-stamp

FORCE:

define lint-command =
cabal exec hlint -- lint $(1)
endef

.PHONY: lint
lint: FORCE
	$(foreach src,$(SRCS),$(call lint-command,$(src)))

.PHONY: run
run: FORCE
	cabal run -- ./.private/config.yaml

.PHONY: repl
repl: FORCE
	cabal repl --ghc-options="-Wwarn"

.PHONY: watch
watch: FORCE
	cabal exec ghcid -- --command='cabal repl --ghc-options="-Wwarn"'

.PHONY: clean
clean: FORCE
	cabal clean
	rm .*-stamp

.PHONY: devtools
devtools: FORCE
	cabal install hlint stylish-haskell hindent
