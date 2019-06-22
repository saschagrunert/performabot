BUILD_DIR := build
GLOB_SCSS := config/bulma.scss
BULMA_DIR := $(BUILD_DIR)/bulma
BULMA_TAG := 0.7.5
BULMA_URL := https://github.com/jgthms/bulma
CONTAINER_RUNTIME := podman

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

define image-build
	$(CONTAINER_RUNTIME) build --pull --no-cache \
		-f Dockerfile-$(1) -t performabot-$(1) .
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
	$(CONTAINER_RUNTIME) run --rm -it -v $(shell pwd):/$$WORKDIR \
		saschagrunert/performabot-build sh -c "\
			export BUILD_DIR=$$WORKDIR/result/bin &&\
			rm -rf $$WORKDIR/result &&\
			mkdir -p \$$BUILD_DIR &&\
			nix-build $$WORKDIR/nix/release-static.nix &&\
			cp result/bin/* \$$BUILD_DIR"

.PHONY: cabal2nix
cabal2nix:
	$(call nix-shell-pure-run,\
		cd nix && \
		cabal2nix --no-haddock .. > default.nix)

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
	$(call image-build,build)

.PHONY: image-client
image-client:
	$(nix-shell-pure-run,hack/is-static result/bin/client)
	$(call image-build,client)

.PHONY: image-server
image-server:
	$(nix-shell-pure-run,hack/is-static result/bin/server)
	$(call image-build,server)

.PHONY: lint
lint: bulma cabal2nix floskell hlint
	$(call nix-shell-pure-run,git diff --exit-code)

.PHONY: locale
locale:
	@$(call nix-shell-pure-run,echo \$$LOCALE_ARCHIVE)

.PHONY: nixpkgs
nixpkgs:
	nix-shell -p nix-prefetch-git --run "nix-prefetch-git --no-deepClone \
		https://github.com/nixos/nixpkgs > nix/nixpkgs.json"

.PHONY: repl-client
repl-client:
	$(call nix-shell-pure-run,cabal new-repl exe:client)

.PHONY: repl-server
repl-server:
	$(call nix-shell-run,hack/repl)

.PHONY: shell
shell:
	$(call nix-shell-pure)

.PHONY: test
test:
	$(call nix-shell-pure-run,\
		cabal new-test --enable-coverage --enable-library-coverage)

.PHONY: bulma
bulma:
	$(call nix-shell-run,\
		if [ ! -d $(BULMA_DIR) ]; then \
			mkdir -p $(BUILD_DIR) &&\
			wget -qO- $(BULMA_URL)/archive/$(BULMA_TAG).tar.gz \
				| tar xfz - -C $(BUILD_DIR) &&\
			mv $(BUILD_DIR)/bulma-* $(BULMA_DIR) ;\
		fi &&\
		sass -t compressed $(GLOB_SCSS) > static/css/bulma.min.css)

.PHONY: yesod
yesod:
	$(call nix-shell-run,stack exec --no-nix-pure -- yesod devel)
