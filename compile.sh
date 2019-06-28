#!/usr/bin/env bash

rm -rf elm-stuff
elm make src/cli/Main.elm --output /dev/null
