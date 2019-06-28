BUILD_DIR := build

define nix-shell
	nix-shell nix/shell.nix $(1)
endef

define nix-shell-pure
	$(call nix-shell,--pure $(1))
endef

define nix-shell-run
	$(call nix-shell,--run "$(1)")
endef

define nix-shell-pure-run
	$(call nix-shell-pure,--run "$(1)")
endef

define image
	$(call nix-shell-pure-run,\
		hack/podman-config &&\
		podman --config=$(BUILD_DIR)/podman.conf --storage-driver=vfs \
			build --pull --no-cache -f image-$(1) -t $(1) &&\
		rm -f $(BUILD_DIR)/$(1).tar &&\
		podman --config=$(BUILD_DIR)/podman.conf --storage-driver=vfs \
			save  -o $(BUILD_DIR)/image-$(1).tar $(1))
endef


all: cabal2nix build

.PHONY: build
build:
	nix-build nix/release.nix

.PHONY: build-static
build-static:
	nix-build nix/release-static.nix

.PHONY: build-static-with-image
build-static-with-image:
	export WORKDIR=/performabot &&\
	podman run --rm -it -v $(shell pwd):/$$WORKDIR \
		saschagrunert/performabot-build sh -c "\
			export BUILD_DIR=$$WORKDIR/result/bin &&\
			rm -rf $$WORKDIR/result &&\
			mkdir -p \$$BUILD_DIR &&\
			nix-build $$WORKDIR/nix/release-static.nix &&\
			cp result/bin/* \$$BUILD_DIR"

.PHONY: cabal2nix
cabal2nix:
	$(call nix-shell-pure-run,cd nix && cabal2nix .. > default.nix)

.PHONY: clean
clean:
	$(call nix-shell-pure-run,git clean -fdx)

.PHONY: coverage
coverage:
	$(call nix-shell-run,\
		cabal configure --enable-tests --enable-coverage &&\
		cabal test &&\
		hpc-coveralls performabot-test \
			-r ehCDKUtRSiNfht5xyC580BaZqCCjSBICz \
			--exclude-dir=test --exclude-dir=dist)

.PHONY: doc
doc:
	$(call nix-shell-pure-run,cabal new-haddock)

.PHONY: floskell
floskell:
	$(call nix-shell-pure-run,shopt -s globstar && floskell src/**/*.hs)

.PHONY: hlint
hlint:
	$(call nix-shell-pure-run,hlint -g)

.PHONY: image-build
image-build:
	$(call image,performabot-build)

.PHONY: image-performabot
image-performabot:
	$(call nix-shell-pure-run,hack/is-static result/bin/performabot)
	$(call image,performabot)

.PHONY: lint
lint: cabal2nix floskell hlint
	$(call nix-shell-pure-run,git diff --exit-code)

.PHONY: locale
locale:
	@$(call nix-shell-pure-run,echo \$$LOCALE_ARCHIVE)

.PHONY: nixpkgs
nixpkgs:
	nix-shell -p nix-prefetch-git --run "nix-prefetch-git --no-deepClone \
		https://github.com/nixos/nixpkgs > nix/nixpkgs.json"

.PHONY: repl
repl:
	$(call nix-shell-pure-run,cabal new-repl)

.PHONY: shell
shell:
	$(call nix-shell-pure)

.PHONY: test
test:
	$(call nix-shell-pure-run,\
		cabal new-test --enable-coverage --enable-library-coverage)
