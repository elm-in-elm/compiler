#!/usr/bin/env bash

rm -rf elm-stuff
elm make src/Main.elm --output /dev/null
