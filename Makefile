.PHONY: cabal2nix shell

all:
	nix-build release.nix

cabal2nix:
	nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix

shell:
	nix-shell --pure shell.nix
