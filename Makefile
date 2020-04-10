.PHONY: build docs test
build: | node_modules
	npx spago build

docs: build
	npx spago docs

test: | node_modules
	npx spago -x test.spago.dhall test

node_modules:
	npm install
