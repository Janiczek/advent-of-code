#!/usr/bin/env bash

YEAR="${1}"
DAY="${2}"
INPUT="src/Year${YEAR}/Day${DAY}.roc"

chokidar '**/*.roc' | while read WHATEVER; do
  clear && tput reset && echo -en "\033c\033[3J"
  roc test "${INPUT}" && roc dev "${INPUT}"
done;
