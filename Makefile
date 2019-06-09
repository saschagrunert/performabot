BUILD_DIR := build
GLOB_SCSS := uikit.scss
UIKIT_DIR := $(BUILD_DIR)/uikit
UIKIT_TAG := 3.1.5
UIKIT_URL := https://github.com/uikit/uikit

all: cabal2nix build

.PHONY: build
build:
	nix-build nix/release.nix

.PHONY: build-static
build-static:
	nix-build nix/static.nix

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

.PHONY: fmt
fmt:
	floskell src/*.hs

.PHONY: hlint
hlint:
	$(call nix-shell-pure-run,hlint -g)

.PHONY: lint
lint: cabal2nix hlint uikit
	$(call nix-shell-pure-run,git diff --exit-code)

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

.PHONY: uikit
uikit:
	$(call nix-shell-run,\
		if [ ! -d $(UIKIT_DIR) ]; then \
			mkdir -p $(BUILD_DIR) &&\
			wget -qO- $(UIKIT_URL)/archive/v$(UIKIT_TAG).tar.gz \
				| tar xfz - -C $(BUILD_DIR) &&\
			mv $(BUILD_DIR)/uikit-* $(UIKIT_DIR) ;\
		fi &&\
		cp config/$(GLOB_SCSS) $(BUILD_DIR) &&\
		cd $(BUILD_DIR) &&\
		sass -t compressed $(GLOB_SCSS) > ../static/css/uikit.min.css)

.PHONY: yesod
yesod:
	$(call nix-shell-run,stack exec -- yesod devel)
