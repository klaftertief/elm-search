.PHONY: install server watch clean test help

# Add binaries of local npm packages to the PATH
PATH := $(PWD)/bin:$(PWD)/node_modules/.bin:$(PATH)
SHELL := /bin/bash

ELM_ENTRY = src/scripts/Web.elm
ELM_FILES = $(shell find src -type f -name '*.elm')

DEVD_VERSION = 0.5
MODD_VERSION = 0.3
ELM_TEST_VERSION = 0.16
OS := $(shell uname)

BUILD_DIR = ../elm-search-dist
INSTALL_TARGETS = bin bin/modd bin/devd node_modules
COMPILE_TARGETS = scripts styles html
TEST_TARGETS = tests/TestRunner.elm

ifeq ($(OS),Darwin)
	DEVD_URL = "https://github.com/cortesi/devd/releases/download/v${DEVD_VERSION}/devd-${DEVD_VERSION}-osx64.tgz"
	MODD_URL = "https://github.com/cortesi/modd/releases/download/v${MODD_VERSION}/modd-${MODD_VERSION}-osx64.tgz"
else
	DEVD_URL = "https://github.com/cortesi/devd/releases/download/v${DEVD_VERSION}/devd-${DEVD_VERSION}-linux64.tgz"
	MODD_URL = "https://github.com/cortesi/modd/releases/download/v${MODD_VERSION}/modd-${MODD_VERSION}-linux64.tgz"
endif

all: $(COMPILE_TARGETS) ## Compiles project files

install: $(INSTALL_TARGETS) ## Installs prerequisites and generates file/folder structure

server: ## Runs a local server for development
	devd  -l -a -p 8888 -w $(BUILD_DIR) $(BUILD_DIR)/


watch: ## Watches files for changes, runs a local dev server and triggers live reload
	modd

# clean: ## Removes compiled files
# 	rm -rf $(BUILD_DIR)/*

help: ## Prints a help guide
	@echo "Available tasks:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

bin:
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

styles:
	mkdir -p $(BUILD_DIR)/styles && cp src/styles/search.css $(BUILD_DIR)/styles

scripts: $(ELM_FILES)
	elm-make $(ELM_ENTRY) --yes --warn --output $(BUILD_DIR)/scripts/search.js

html:
	mkdir -p $(BUILD_DIR) && cp src/index.html $(BUILD_DIR)/index.html
