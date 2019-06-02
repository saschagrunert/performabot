all:
	nix-build release.nix

.PHONY: cabal2nix
cabal2nix:
	nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix

.PHONY: fmt
fmt:
	floskell src/*.hs

.PHONY: lint
lint: cabal2nix
	nix-shell --pure -p git --run "git diff --exit-code"

.PHONY: nixpkgs
nixpkgs:
	nix-shell -p nix-prefetch-git --run "nix-prefetch-git --no-deepClone \
		https://github.com/nixos/nixpkgs > nixpkgs.json"

.PHONY: shell
shell:
	nix-shell --pure
