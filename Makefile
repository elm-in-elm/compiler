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

.PHONY: watch
watch:
	@clear; tput reset; echo "Watch mode started..."; inotifywait -mqr -e close_write --format '%w %e %f' src | while read DIR EVENT FILE; do clear; tput reset; rm -rf elm-stuff; sysconfcpus -n 1 ./node_modules/.bin/elm make src/Main.elm --output /dev/null; done
