#!/usr/bin/env bash

INPUT="Year${1}/Day${2}.elm" 
OUTPUT="js/Year${1}-Day${2}.js"
NODE="Year${1}/Day${2}.js"

clear;
elm-make --yes --warn "${INPUT}" --output "${OUTPUT}";
echo "--------------------------------------";
node "${NODE}";
