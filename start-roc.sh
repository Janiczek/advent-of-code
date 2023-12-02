#!/usr/bin/env bash

YEAR="${1}"
DAY="${2}"
DAY_NO_ZEROS="$(echo $DAY | sed 's/^0*//')"
INPUT="Template.roc"
OUTPUT="src/Year${YEAR}/Day${DAY}.roc"
PUZZLE_URL="https://adventofcode.com/${YEAR}/day/${DAY_NO_ZEROS}/input"
PUZZLE_FILE="input${YEAR}${DAY}.txt"

curl "${PUZZLE_URL}" -H "cookie: session=${AOC_SESSION_COOKIE}" -o "${PUZZLE_FILE}" 2>/dev/null
cat "${PUZZLE_FILE}"
mkdir -p "$(dirname ${OUTPUT})"
cp "${INPUT}" "${OUTPUT}"
gsed -i "s/inputXXX.txt/${PUZZLE_FILE}/g" "${OUTPUT}"

./watch-roc.sh "${YEAR}" "${DAY}"
