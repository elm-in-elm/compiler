# elm-format does not support excluding files so we explicitly list all the directories
# we want to be formatted. See https://github.com/avh4/elm-format/issues/669.
FORMAT_DIRS = \
	benchmarks \
	cli \
	example-library-usages \
	src \
	tests

.PHONY: run
run: build
	tput reset
	cd example-project && \
	(node $(NODE_FLAGS) ../cli/index.js --main src/Main.elm || true) \
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

.SILENT: test-perf
.PHONY: test-perf
test-perf:
	rm -rf build/elm.js
	echo "COMPILING THE COMPILER (NON-OPTIMIZED)"
	cd cli && npx elm make Main.elm --output ../build/elm.js
	echo "MEASURING! (NON-OPTIMIZED)"
	bash -c "cd example-project && time node ../cli/index.js --main src/Main.elm"
	echo -e "\nCOMPILING THE COMPILER (OPTIMIZED)"
	cd cli && npx elm make Main.elm --optimize --output ../build/elm.js
	echo "MEASURING! (OPTIMIZED)"
	bash -c "cd example-project && time node ../cli/index.js --main src/Main.elm"
