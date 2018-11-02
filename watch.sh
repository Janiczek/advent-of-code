#!/usr/bin/env bash

YEAR="${1}"
DAY="${2}"
INPUT="Year${YEAR}/Day${DAY}.elm"
OUTPUT="js/${YEAR}-${DAY}.js"

while true; do
  ls "Advent.elm" "${INPUT}" | entr -d ./run.sh "${1}" "${2}";
done;

