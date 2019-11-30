#!/usr/bin/env bash

YEAR="${1}"
DAY="${2}"
DAY_NO_ZEROS="$(echo $DAY | sed 's/^0*//')"
INPUT="Template.elm"
OUTPUT="Year${YEAR}/Day${DAY}.elm"
PUZZLE_URL="https://adventofcode.com/${YEAR}/day/${DAY_NO_ZEROS}/input"
PUZZLE_FILE="input${YEAR}${DAY}.txt"

curl "${PUZZLE_URL}" -H "cookie: session=${AOC_SESSION_COOKIE}" -o "${PUZZLE_FILE}" 2>/dev/null
mkdir -p "$(dirname ${OUTPUT})"
cp "${INPUT}" "${OUTPUT}"
sed -i "s/YearXXX/Year${YEAR}/g" "${OUTPUT}"
sed -i "s/DayXXX/Day${DAY}/g" "${OUTPUT}"
sed -i -e "/InputXXX/r ${PUZZLE_FILE}" -e "/InputXXX/d" "${OUTPUT}"

./watch.sh "${YEAR}" "${DAY}"
