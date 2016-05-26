.PHONY: install server watch clean help build download local publish

# Add binaries of local npm packages to the PATH
PATH := $(PWD)/bin:$(PWD)/node_modules/.bin:$(PATH)
SHELL := /bin/bash

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
	DEVD_URL = "https://github.com/cortesi/devd/releases/download/v${DEVD_VERSION}/devd-${DEVD_VERSION}-osx64.tgz"
	MODD_URL = "https://github.com/cortesi/modd/releases/download/v${MODD_VERSION}/modd-${MODD_VERSION}-osx64.tgz"
	JQ_URL := "https://github.com/stedolan/jq/releases/download/jq-${JQ_VERSION}/jq-osx-amd64"
else
	DEVD_URL = "https://github.com/cortesi/devd/releases/download/v${DEVD_VERSION}/devd-${DEVD_VERSION}-linux64.tgz"
	MODD_URL = "https://github.com/cortesi/modd/releases/download/v${MODD_VERSION}/modd-${MODD_VERSION}-linux64.tgz"
	JQ_URL := "https://github.com/stedolan/jq/releases/download/jq-${JQ_VERSION}/jq-linux64"
endif

publishedPackages = $(shell curl http://package.elm-lang.org/all-packages?elm-package-version=$(1) | jq -r '.[] | "$(BUILD_DIR)/packages/" + .name + "/" + .versions[0] + "/documentation.json"')

PUBLISHED_PACKAGES_016 = $(call publishedPackages,0.16)
PUBLISHED_PACKAGES_017 = $(call publishedPackages,0.17)
LOCAL_PACKAGES = $(shell <elm-stuff/exact-dependencies.json jq -r 'to_entries | .[] | "$(BUILD_DIR)/packages/" + .key + "/" + .value + "/documentation.json"')


build: $(BUILD_DIR) $(COMPILE_TARGETS) ## Compiles project files

download: $(BUILD_DIR) $(BUILD_DIR)/index-published-0.17.json $(BUILD_DIR)/index-published-0.16.json ## Downloads docs files

local: $(BUILD_DIR) $(BUILD_DIR)/index-published-0.17.json $(BUILD_DIR)/index-published-0.16.json ## Downloads docs files from locally installed packages

publish: build download ## Builds the app, downloads packages, makes a new commit in the `gh-pages` branch and pushes it to GitHub
	(cd $(BUILD_DIR) && git add -A && git commit -m "Update app and documentation" && git push origin gh-pages)

install: $(INSTALL_TARGETS) ## Installs prerequisites and generates file/folder structure

server: ## Runs a local server for development
	devd  -l -a -p 8888 -w $(BUILD_DIR) $(BUILD_DIR)/

watch: ## Watches files for changes, runs a local dev server and triggers live reload
	modd

clean: ## Removes compiled files
	rm -rf $(BUILD_DIR)/*

help: ## Prints a help guide
	@echo "Available tasks:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

styles:
	mkdir -p $(BUILD_DIR)/styles && cp src/styles/search.css $(BUILD_DIR)/styles

scripts: $(ELM_FILES)
	elm-make $(ELM_ENTRY) --yes --warn --output $(BUILD_DIR)/scripts/search.js

html:
	mkdir -p $(BUILD_DIR) && cp -r src/html/ $(BUILD_DIR)

# TODO: find a nice way to keep this DRY and dynamic
$(BUILD_DIR)/0.17/index.json: $(PUBLISHED_PACKAGES_017)
	@mkdir -p $(BUILD_DIR)/0.17
	@jq '(input_filename|ltrimstr("$(BUILD_DIR)/packages/")|rtrimstr("/documentation.json")|capture("(?<name>^.+)\/(?<version>\\d\\.\\d\\.\\d$$)")) + {docs: .} | select(.docs[0]["generated-with-elm-version"] | startswith("0.17"))?' $^ | jq -s '.' > $@

$(BUILD_DIR)/0.16/index.json: $(PUBLISHED_PACKAGES_016)
	@mkdir -p $(BUILD_DIR)/0.16
	@jq '(input_filename|ltrimstr("$(BUILD_DIR)/packages/")|rtrimstr("/documentation.json")|capture("(?<name>^.+)\/(?<version>\\d\\.\\d\\.\\d$$)")) + {docs: .} | select(.docs[0]["generated-with-elm-version"] | startswith("0.16"))?' $^ | jq -s '.' > $@

$(BUILD_DIR)/local/index.json: $(LOCAL_PACKAGES)
	@mkdir -p $(BUILD_DIR)/local
	@jq '(input_filename|ltrimstr("$(BUILD_DIR)/packages/")|rtrimstr("/documentation.json")|capture("(?<name>^.+)\/(?<version>\\d\\.\\d\\.\\d$$)")) + {docs: .}' $^ | jq -s '.' > $@

$(BUILD_DIR)/packages/%/documentation.json:
	curl $(ELM_PACKAGE_URL)/$(@:$(BUILD_DIR)/%=%) -o $@ -f --retry 2 --create-dirs -L

elm-stuff/exact-dependencies.json:
	elm-package install --yes

# elm-stuff/packages/%/documentation.json:
# 	pushd $(dir $@) && elm-make --docs documentation.json --yes && popd

bin $(BUILD_DIR):
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

