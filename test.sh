#!/usr/bin/env bash

COLOR_OFF="\e[0m";
DIM="\e[2m";

function run {
  clear;
  tput reset;

  echo -en "${DIM}";
  date -R;
  echo -en "${COLOR_OFF}";

  cd src/cli && elm-test && cd ../library && elm-test && cd ../..;
}

run;

inotifywait -mqr -e close_write --format '%w %e %f' src tests | while read DIR EVENT FILE; do
  run;
done;
