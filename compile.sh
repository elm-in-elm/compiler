#!/usr/bin/env bash

rm -rf elm-stuff
sysconfcpus -n 1 yarn elm make src/Main.elm --output /dev/null
