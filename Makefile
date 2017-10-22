.PHONY: install server watch clean help setup build publish

ELM_TMP_DIR := elm-stuff/generated-code/klaftertief/elm-search
ELM_SETUP := src/scripts/Setup.elm
ELM_SETUP_COMPILED := $(ELM_TMP_DIR)/setup.js
ELM_MAIN := $(ELM_TMP_DIR)/Main.elm
ELM_FILES = $(shell find src -type f -name '*.elm')

DEVD_VERSION := 0.5
MODD_VERSION := 0.3
OS := $(shell uname)

BUILD_DIR := dist
CACHE_DIR := cache
INSTALL_TARGETS := bin bin/modd bin/devd node_modules
COMPILE_TARGETS := scripts styles html

ifeq ($(OS),Darwin)
	DEVD_URL := "https://github.com/cortesi/devd/releases/download/v${DEVD_VERSION}/devd-${DEVD_VERSION}-osx64.tgz"
	MODD_URL := "https://github.com/cortesi/modd/releases/download/v${MODD_VERSION}/modd-${MODD_VERSION}-osx64.tgz"
else
	DEVD_URL := "https://github.com/cortesi/devd/releases/download/v${DEVD_VERSION}/devd-${DEVD_VERSION}-linux64.tgz"
	MODD_URL := "https://github.com/cortesi/modd/releases/download/v${MODD_VERSION}/modd-${MODD_VERSION}-linux64.tgz"
endif

package: clean install setup build ## Creates the deployment artifact

setup: $(ELM_TMP_DIR) ## Downloads docs files
	node_modules/.bin/elm-make $(ELM_SETUP) --yes --warn --output $(ELM_SETUP_COMPILED)
	src/scripts/index.js $(ELM_SETUP_COMPILED) $(CACHE_DIR) $(ELM_MAIN)

build: $(BUILD_DIR) $(COMPILE_TARGETS) ## Compiles project files

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
	mkdir -p $(BUILD_DIR)/styles
	cp src/styles/search.css $(BUILD_DIR)/styles

scripts: $(ELM_FILES)
	node_modules/.bin/elm-make $(ELM_MAIN) --yes --output $(BUILD_DIR)/scripts/search.js
	node_modules/.bin/uglifyjs $(BUILD_DIR)/scripts/search.js --output $(BUILD_DIR)/scripts/search.js

html:
	mkdir -p $(BUILD_DIR)
	cp -r src/html/* $(BUILD_DIR)

bin $(BUILD_DIR) $(CACHE_DIR) $(ELM_TMP_DIR):
	mkdir -p $@

bin/devd:
	curl ${DEVD_URL} -L -o $@.tgz
	tar -xzf $@.tgz -C bin/ --strip 1
	rm $@.tgz

bin/modd:
	curl ${MODD_URL} -L -o $@.tgz
	tar -xzf $@.tgz -C bin/ --strip 1
	rm $@.tgz

node_modules:
	npm install
