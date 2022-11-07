
# elm-format does not support excluding files so we explicitly list all the directories
# we want to be formatted. See https://github.com/avh4/elm-format/issues/669.
FORMAT_DIRS = \
	benchmarks \
	cli \
	example-library-usages \
	src \
	tests

.SILENT: run
.PHONY: run
run: build
	tput reset
	cd example-project && \
	(node $(NODE_FLAGS) ../cli/index.js --main src/Main.elm -o hvm || true) \
	# have `make run` succeed even if compilation fails.

.PHONY: json
json: build
	tput reset
	cd example-project && node ../cli/index.js --main src/Main.elm -o json

.PHONY: python
python: build
	tput reset
	cd example-project && node ../cli/index.js --main src/Main.elm -o python

.PHONY: build
build:
	rm -rf build/elm.js elm-stuff
	cd cli && npx elm make Main.elm --output ../build/elm.js

.PHONY: watch
watch:
	cd cli && npx elm-live Main.elm --no-server

.PHONY: test
test: build
	npx elm make --output /dev/null # build the library just to test it compiles
	npx elm-test
	npx ava

.PHONY: format
format:
	npx elm-format $(FORMAT_DIRS) --yes
	npx xo --fix

.PHONY: lint
lint:
	npx elm-format $(FORMAT_DIRS) --validate
	npx xo

.PHONY: readme_lib
readme_lib:
	mv README.md README-github.md
	mv README-library.md README.md

.PHONY: readme_gh
readme_gh:
	mv README.md README-library.md
	mv README-github.md README.md

.PHONY: review
review:
	npx elm-review
