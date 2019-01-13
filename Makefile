.PHONY: run
run: build
	node src/index.js

.PHONY: build
build: node_modules/.bin/elm
	rm -rf build/elm.js elm-stuff
	./node_modules/.bin/elm make src/Main.elm --output build/elm.js

node_modules/.bin/elm:
	yarn

.PHONY: watch
watch:
	elm-watch src/Main.elm src/
