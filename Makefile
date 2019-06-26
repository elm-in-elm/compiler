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
	elm-test
