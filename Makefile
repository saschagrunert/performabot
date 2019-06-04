all: build

.PHONY: build
build:
	nix-build nix/release.nix

.PHONY: build-static
build-static:
	nix-build nix/static.nix

define nix-shell
	nix-shell nix/shell.nix --pure $(1)
endef

define nix-shell-run
	$(call nix-shell,--run "$(1)")
endef

.PHONY: cabal2nix
cabal2nix:
	$(call nix-shell-run,cd nix && cabal2nix .. > default.nix)

.PHONY: clean
clean:
	$(call nix-shell-run,git clean -fdx)

.PHONY: coverage
coverage:
	nix-shell nix/shell.nix --run \
		"cabal configure --enable-tests --enable-coverage &&\
		 cabal test &&\
		 hpc-coveralls performabot-test \
			-r ehCDKUtRSiNfht5xyC580BaZqCCjSBICz \
			--exclude-dir=test"

.PHONY: doc
doc:
	$(call nix-shell-run,cabal new-haddock)

.PHONY: fmt
fmt:
	floskell src/*.hs

.PHONY: hlint
hlint:
	$(call nix-shell-run,hlint -g)

.PHONY: lint
lint: cabal2nix hlint
	$(call nix-shell-run,git diff --exit-code)

.PHONY: nixpkgs
nixpkgs:
	nix-shell -p nix-prefetch-git --run "nix-prefetch-git --no-deepClone \
		https://github.com/nixos/nixpkgs > nix/nixpkgs.json"

.PHONY: repl
repl:
	$(call nix-shell-run,cabal new-repl exe:performabot)

.PHONY: shell
shell:
	$(call nix-shell)

.PHONY: test
test:
	$(call nix-shell-run,\
		cabal new-test --enable-coverage --enable-library-coverage)
