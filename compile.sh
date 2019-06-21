#!/usr/bin/env bash

rm -rf elm-stuff
yarn elm make src/Main.elm --output /dev/null
