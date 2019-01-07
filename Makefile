.PHONY: run
run: build
	node src/index.js

.PHONY: build
build:
	rm -rf build/elm.js elm-stuff
	./node_modules/.bin/elm make src/Main.elm --output build/elm.js
