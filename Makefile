
# elm-format does not support excluding files so we explicitly list all the directories
# we want to be formatted. See https://github.com/avh4/elm-format/issues/669.
FORMAT_DIRS = \
	benchmarks \
	cli \
	example-library-usages \
	example-project \
	integration-tests/cli \
	integration-tests/parser \
	integration-tests/desugar \
	integration-tests/typecheck \
	src \
	tests

.PHONY: run
run: build
	tput reset
	cd example-project && \
	(node ../cli/index.js --main src/Main.elm || true) \
	# have `make run` succeed even if compilation fails.

.PHONY: json
json: build
	tput reset
	cd example-project && node ../cli/index.js --main src/Main.elm -o JSON

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

.PHONY: lint
lint:
	npx elm-format $(FORMAT_DIRS) --validate

.PHONY: readme_lib
readme_lib:
	mv README.md README-github.md
	mv README-library.md README.md

.PHONY: readme_gh
readme_gh:
	mv README.md README-library.md
	mv README-github.md README.md

# Running elm-review without args can lead to false positives about unused stuff, which is in fact used in tests
# To fix that, we search for all committed elm files and pass them explicitly to elm-review via xargs
.PHONY: review
review:
	git ls-files '*.elm' | grep -vE "(integration-tests|benchmarks|example-*)" | xargs npx elm-review
