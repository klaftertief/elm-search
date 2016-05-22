.PHONY: install server watch clean help build download all-packages publish

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

PACKAGE_DOCS_TARGETS = $(shell <$(BUILD_DIR)/all-packages.json bin/jq -r '.[] | "$(BUILD_DIR)/packages/" + .name + "/" + .versions[0] + "/documentation.json"')

ifeq ($(OS),Darwin)
	DEVD_URL = "https://github.com/cortesi/devd/releases/download/v${DEVD_VERSION}/devd-${DEVD_VERSION}-osx64.tgz"
	MODD_URL = "https://github.com/cortesi/modd/releases/download/v${MODD_VERSION}/modd-${MODD_VERSION}-osx64.tgz"
	JQ_URL := "https://github.com/stedolan/jq/releases/download/jq-${JQ_VERSION}/jq-osx-amd64"
else
	DEVD_URL = "https://github.com/cortesi/devd/releases/download/v${DEVD_VERSION}/devd-${DEVD_VERSION}-linux64.tgz"
	MODD_URL = "https://github.com/cortesi/modd/releases/download/v${MODD_VERSION}/modd-${MODD_VERSION}-linux64.tgz"
	JQ_URL := "https://github.com/stedolan/jq/releases/download/jq-${JQ_VERSION}/jq-linux64"
endif

build: $(BUILD_DIR) $(COMPILE_TARGETS) ## Compiles project files

download: $(BUILD_DIR) package-docs $(BUILD_DIR)/all-package-docs.json ## Downloads docs files

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
	mkdir -p $(BUILD_DIR) && cp src/index.html $(BUILD_DIR)/index.html


$(BUILD_DIR)/all-docs.json: all-packages
	@bin/jq -s . $(PACKAGE_DOCS_TARGETS) > $@

$(BUILD_DIR)/all-package-docs.json: all-packages $(BUILD_DIR)/all-docs.json
	@jq -n --slurpfile packages $(BUILD_DIR)/$(word 1,$^).json --slurpfile docs $(word 2,$^) '$$packages[0] | to_entries | map(.value + {version: .value.versions[0], docs: $$docs[0][.key]} | del(.versions))' > $@

package-docs: all-packages
	@$(MAKE) $(PACKAGE_DOCS_TARGETS)

%-packages:
	curl $(ELM_PACKAGE_URL)/$@ -o $(BUILD_DIR)/$@.json -f --retry 2 --create-dirs

$(BUILD_DIR)/packages/%/documentation.json:
	curl $(ELM_PACKAGE_URL)/$(@:$(BUILD_DIR)/%=%) -o $@ -f --retry 2 --create-dirs -L

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

