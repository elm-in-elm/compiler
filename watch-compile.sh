#!/usr/bin/env bash

COLOR_OFF="\e[0m";
DIM="\e[2m";

LOCKNAME=$(cat /dev/urandom | tr -cd 'a-f0-9' | head -c 16);
LOCKFILE="/tmp/elm-lock-${LOCKNAME}"

function compile {
	cd cli && elm make Main.elm --output ../build/elm.js
}

function run {
  (
  flock 200

  clear;
  tput reset;

  echo -en "${DIM}";
  date -R;
  echo -en "${COLOR_OFF}";

  compile;

  ) 200>"${LOCKFILE}"
}

run;

find . -name '*.elm' | xargs inotifywait -mqr -e close_write --format '%w %e %f' | while read DIR EVENT FILE; do
  run;
done;
