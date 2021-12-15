#!/usr/bin/env bash

YEAR="${1}"
DAY="${2}"
INPUT="Year${YEAR}/Day${DAY}.elm"
OUTPUT="js/${YEAR}-${DAY}.js"
MODULE="Year${YEAR}.Day${DAY}"



clear;
tput reset;
echo -en "\033c\033[3J";
elm make "${INPUT}" --output "${OUTPUT}" && \
  echo "let oldLog=console.log;console.log=function(...args){let x=args[0];if(x.startsWith('[START]')){console.time('time');}else if(x.startsWith('[LOG]')){console.timeLog('time');}else if(x.startsWith('[END]')){console.timeEnd('time');}else{oldLog(...args);}}" >>"${OUTPUT}" && \
echo "this.Elm.${MODULE}.init();" >>"${OUTPUT}" && \
echo "--------------------------------------" && \
node --max-old-space-size=12288 "${OUTPUT}";
#echo "try{this.Elm.${MODULE}.init();}catch(e){console.log(e.message);}" >>"${OUTPUT}" && \
