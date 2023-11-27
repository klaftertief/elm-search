SHELL := /bin/bash

.PHONY: install server watch clean help setup build publish

ELM_TMP_DIR := elm-stuff/generated-code/klaftertief/elm-search
ELM_SETUP := src/scripts/Setup.elm
ELM_SETUP_COMPILED := $(ELM_TMP_DIR)/setup.js
ELM_MAIN := $(ELM_TMP_DIR)/Main.elm
ELM_FILES = $(shell find src -type f -name '*.elm')

BUILD_DIR := dist
CACHE_DIR := cache
COMPILE_TARGETS := scripts styles html

help: ## Prints a help guide
	@echo "Available tasks:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

setup: ## Downloads docs files
	mkdir -p $(ELM_TMP_DIR)
	mkdir -p $(CACHE_DIR)
	node_modules/.bin/elm make $(ELM_SETUP) --output $(ELM_SETUP_COMPILED)
	src/scripts/index.js $(ELM_SETUP_COMPILED) $(CACHE_DIR) $(ELM_MAIN)

build: $(COMPILE_TARGETS) ## Compiles project files

install: ## Installs dependencies
	npm install

clean: ## Removes compiled files
	rm -rf $(BUILD_DIR)/*

styles:
	mkdir -p $(BUILD_DIR)/styles
	cp src/styles/search.css $(BUILD_DIR)/styles

scripts: $(ELM_FILES)
	node_modules/.bin/elm make $(ELM_MAIN) --optimize --output $(BUILD_DIR)/scripts/search.js

html:
	mkdir -p $(BUILD_DIR)
	cp -r src/html/* $(BUILD_DIR)
