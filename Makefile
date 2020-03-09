.PHONY: run
run: build
	tput reset
	@ cd example-project && \
	(node ../cli/index.js --main src/Main.elm || true) \
	# have `make run` succeed even if compilation fails.

.PHONY: json
json: build
	tput reset
	@ cd example-project && node ../cli/index.js --main src/Main.elm -o JSON

.PHONY: build
build:
	rm -rf build/elm.js elm-stuff
	cd cli && elm make Main.elm --output ../build/elm.js

.PHONY: test
test: build
	elm make --output /dev/null # build the library just to test it compiles
	elm-test
	npx ava

.PHONY: format
format:
	elm-format . --yes


.PHONY: lint
lint:
	elm-format . --validate

.PHONY: readme_lib
readme_lib:
	mv README.md README-github.md
	mv README-library.md README.md

.PHONY: readme_gh
readme_gh:
	mv README.md README-library.md
	mv README-github.md README.md
