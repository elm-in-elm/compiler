.PHONY: run
run: build
	tput reset
	@node src/index.js

.PHONY: build
build:
	rm -rf build/elm.js elm-stuff
	elm make src/Main.elm --output build/elm.js

.PHONY: test
test:
	elm-test
