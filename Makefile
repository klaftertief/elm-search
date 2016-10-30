.PHONY: install server watch clean help build download local publish cache/packages-017 cache/packages-016

ELM_ENTRY := src/scripts/Web.elm
ELM_FILES = $(shell find src -type f -name '*.elm')

ELM_PACKAGE_URL := http://package.elm-lang.org

DEVD_VERSION := 0.5
MODD_VERSION := 0.3
JQ_VERSION := 1.5
OS := $(shell uname)

BUILD_DIR := dist
INSTALL_TARGETS := bin bin/modd bin/devd bin/jq node_modules
COMPILE_TARGETS := scripts styles html

ifeq ($(OS),Darwin)
	DEVD_URL := "https://github.com/cortesi/devd/releases/download/v${DEVD_VERSION}/devd-${DEVD_VERSION}-osx64.tgz"
	MODD_URL := "https://github.com/cortesi/modd/releases/download/v${MODD_VERSION}/modd-${MODD_VERSION}-osx64.tgz"
	JQ_URL := "https://github.com/stedolan/jq/releases/download/jq-${JQ_VERSION}/jq-osx-amd64"
else
	DEVD_URL := "https://github.com/cortesi/devd/releases/download/v${DEVD_VERSION}/devd-${DEVD_VERSION}-linux64.tgz"
	MODD_URL := "https://github.com/cortesi/modd/releases/download/v${MODD_VERSION}/modd-${MODD_VERSION}-linux64.tgz"
	JQ_URL := "https://github.com/stedolan/jq/releases/download/jq-${JQ_VERSION}/jq-linux64"
endif

publishedPackages = $(shell curl http://package.elm-lang.org/all-packages?elm-package-version=$(1) | bin/jq -r '.[] | "cache/packages/" + .name + "/" + .versions[0] + "/documentation.json"')

LOCAL_PACKAGES = $(shell <elm-stuff/exact-dependencies.json bin/jq -r 'to_entries | .[] | "cache/packages/" + .key + "/" + .value + "/documentation.json"')


build: $(BUILD_DIR) $(COMPILE_TARGETS) ## Compiles project files

download: $(BUILD_DIR) cache $(BUILD_DIR)/0.16/index.json $(BUILD_DIR)/0.17/index.json ## Downloads docs files

local: $(BUILD_DIR) $(BUILD_DIR)/local/index.json ## Downloads docs files from locally installed packages

publish: build download ## Builds the app, downloads packages, makes a new commit in the `gh-pages` branch and pushes it to GitHub
	(cd $(BUILD_DIR) && git add -A && git commit -m "Update app and documentation" && git push origin gh-pages)

install: $(INSTALL_TARGETS) ## Installs prerequisites and generates file/folder structure

server: ## Runs a local server for development
	bin/devd  -l -a -p 8888 -w $(BUILD_DIR) $(BUILD_DIR)/

watch: ## Watches files for changes, runs a local dev server and triggers live reload
	bin/modd

clean: ## Removes compiled files
	rm -rf $(BUILD_DIR)/*

help: ## Prints a help guide
	@echo "Available tasks:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

styles:
	mkdir -p $(BUILD_DIR)/styles && cp src/styles/search.css $(BUILD_DIR)/styles

scripts: $(ELM_FILES)
	node_modules/.bin/elm-make $(ELM_ENTRY) --yes --warn --output $(BUILD_DIR)/scripts/search.js

html:
	mkdir -p $(BUILD_DIR) && cp -r src/html/* $(BUILD_DIR)

# TODO: find a nice way to keep this DRY and dynamic
cache/packages-017:
	@echo $(call publishedPackages,0.17) > $@

$(BUILD_DIR)/0.17/index.json: cache/packages-017
	@mkdir -p $(BUILD_DIR)/0.17
	@$(MAKE) $(shell cat $<)
	@bin/jq '(input_filename|ltrimstr("cache/packages/")|rtrimstr("/documentation.json")|capture("(?<name>^.+)\/(?<version>\\d+\\.\\d+\\.\\d+$$)")) + {docs: .} | select(.docs[0]["generated-with-elm-version"] | startswith("0.17"))?' $(shell cat $<) | bin/jq -s '.' > $@

cache/packages-016: cache
	@echo $(call publishedPackages,0.16) > $@

$(BUILD_DIR)/0.16/index.json: cache/packages-016
	@mkdir -p $(BUILD_DIR)/0.16
	@$(MAKE) $(shell cat $<)
	@bin/jq '(input_filename|ltrimstr("cache/packages/")|rtrimstr("/documentation.json")|capture("(?<name>^.+)\/(?<version>\\d+\\.\\d+\\.\\d+$$)")) + {docs: .} | select(.docs[0]["generated-with-elm-version"] | startswith("0.16"))?' $(shell cat $<) | bin/jq -s '.' > $@

$(BUILD_DIR)/local/index.json: $(LOCAL_PACKAGES)
	@mkdir -p $(BUILD_DIR)/local
	@bin/jq '(input_filename|ltrimstr("cache/packages/")|rtrimstr("/documentation.json")|capture("(?<name>^.+)\/(?<version>\\d+\\.\\d+\\.\\d+$$)")) + {docs: .}' $^ | bin/jq -s '.' > $@

cache/packages/%/documentation.json:
	curl $(ELM_PACKAGE_URL)/$(@:cache/%=%) -o $@ -f --retry 2 --create-dirs -L

elm-stuff/exact-dependencies.json:
	node_modules/.bin/elm-package install --yes

# elm-stuff/packages/%/documentation.json:
# 	pushd $(dir $@) && elm-make --docs documentation.json --yes && popd

bin $(BUILD_DIR) cache:
	mkdir -p $@

bin/devd:
	curl ${DEVD_URL} -L -o $@.tgz
	tar -xzf $@.tgz -C bin/ --strip 1
	rm $@.tgz

bin/modd:
	curl ${MODD_URL} -L -o $@.tgz
	tar -xzf $@.tgz -C bin/ --strip 1
	rm $@.tgz

bin/jq:
	curl ${JQ_URL} -o bin/jq -f --retry 2 --create-dirs -L
	chmod +x bin/jq

node_modules:
	npm install

