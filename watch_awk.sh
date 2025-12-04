#!/usr/bin/env bash

YEAR="${1}"
DAY="${2}"

./run_awk.sh "${1}" "${2}";
chokidar '**/*.awk' | while read WHATEVER; do
  ./run_awk.sh "${1}" "${2}";
done;
