#!/usr/bin/env bash

rm -rf elm-stuff
sysconfcpus -n 1 ./node_modules/.bin/elm make src/Main.elm --output /dev/null
