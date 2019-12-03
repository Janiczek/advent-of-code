#!/usr/bin/env bash

YEAR="${1}"
DAY="${2}"
INPUT="Year${YEAR}/Day${DAY}.elm"
OUTPUT="js/${YEAR}-${DAY}.js"
MODULE="Year${YEAR}.Day${DAY}"



clear;
tput reset;
elm make "${INPUT}" --output "${OUTPUT}" && \
echo "try{this.Elm.${MODULE}.init();}catch(e){console.log(e.message);}" >>"${OUTPUT}" && \
sed -i "s/throw \(new Error('TODO in module.*\)/var error = \\1\n     error.stack = null;\n     throw error;/" "${OUTPUT}" && \
echo "--------------------------------------" && \
node --max-old-space-size=12288 "${OUTPUT}";
