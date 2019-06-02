.PHONY: cabal2nix lint shell

all:
	nix-build release.nix

define nix-shell
    nix-shell --pure -p ${1} --run ${2}
endef

cabal2nix:
	$(call nix-shell,cabal2nix,"cabal2nix ." > default.nix)

fmt:
	$(call nix-shell,floskell,"floskell src/*.hs")

lint: cabal2nix
	$(call nix-shell,git,"git diff --exit-code")

shell:
	nix-shell --pure shell.nix
