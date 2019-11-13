.PHONY: test-perf
test-perf:
	@rm -rf ../build/elm.js ../cli/elm-stuff
	@echo "COMPILING THE COMPILER (NON-OPTIMIZED)"
	@cd cli && elm make Main.elm --output ../build/elm.js >/dev/null 2>&1
	@echo "MEASURING! (NON-OPTIMIZED)"
	@bash -c "time node cli/index.js >/dev/null 2>&1"
	@echo -e "\nCOMPILING THE COMPILER (OPTIMIZED)"
	@cd cli && elm make Main.elm --optimize --output ../build/elm.js >/dev/null 2>&1
	@echo "MEASURING! (OPTIMIZED)"
	@bash -c "time node cli/index.js >/dev/null 2>&1"
	

.PHONY: run
run: build
	@tput reset
	@node cli/index.js

.PHONY: build
build:
	rm -rf build/elm.js cli/elm-stuff
	cd cli && elm make Main.elm --output ../build/elm.js

.PHONY: test
test:
	elm make --output /dev/null # build the library just to test it compiles
	elm-test

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
