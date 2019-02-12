.PHONY: run
run: build
	node src/index.js

.PHONY: build
build: node_modules/.bin/elm
	rm -rf build/elm.js elm-stuff
	./node_modules/.bin/elm make src/Main.elm --output build/elm.js

node_modules/.bin/elm:
	yarn

node_modules/.bin/elm-test:
	yarn

.PHONY: test
test: node_modules/.bin/elm-test
	node_modules/.bin/elm-test
