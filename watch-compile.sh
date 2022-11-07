#!/usr/bin/env bash

COLOR_OFF="\e[0m";
DIM="\e[2m";

function compile {
	#cd cli && elm make Main.elm --output ../build/elm.js
  make run
}

function run {
  clear;
  tput reset;
  echo -en "\033c\033[3J";

  echo -en "${DIM}";
  date -R;
  echo -en "${COLOR_OFF}";

  compile;
}

run;

chokidar '**/*.elm' | while read WHATEVER; do
  run;
done;
