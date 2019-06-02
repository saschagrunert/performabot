.PHONY: cabal2nix lint shell

all:
	nix-build release.nix

cabal2nix:
	nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix

lint: cabal2nix
	nix-shell --pure -p git --run "git diff --exit-code"

shell:
	nix-shell --pure shell.nix
