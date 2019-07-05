.PHONY: run
run: build
	tput reset
	@node src/cli/index.js

.PHONY: build
build:
	rm -rf build/elm.js elm-stuff
	elm make src/cli/Main.elm --output build/elm.js

.PHONY: test
test:
	elm make src/library/Elm/Compiler.elm --output /dev/null # build the library just to test it compiles
	elm-test

.PHONY: format
format:
	elm-format . --yes


.PHONY: lint
lint:
	elm-format . --validate
