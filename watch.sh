#!/usr/bin/env bash

YEAR="${1}"
DAY="${2}"
INPUT="src/Year${YEAR}/Day${DAY}.elm"
OUTPUT="js/${YEAR}-${DAY}.js"

./run.sh "${1}" "${2}";
chokidar '**/*.elm' | while read WHATEVER; do
  ./run.sh "${1}" "${2}";
done;
