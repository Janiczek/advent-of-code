#!/usr/bin/env bash

YEAR="${1}"
DAY="${2}"
CODE="src/Year${YEAR}/Day${DAY}.awk"
EXAMPLE="${EXAMPLE:-0}"
if [ "$EXAMPLE" -eq 1 ]; then
INPUT="input/${YEAR}/${DAY}.example.txt"
else
INPUT="input/${YEAR}/${DAY}.txt"
fi


COLOR_OFF="\e[0m"
DIM="\e[2m"
RED="\e[0;31m"

clear
tput reset
echo -en "\033c\033[3J"
echo -en "${DIM}"
date
echo -en "${COLOR_OFF}"
start_time="$(date +%s%N)"
if fawk -f "${CODE}" "${INPUT}"; then
  OK=1
else
  OK=0
fi
end_time="$(date +%s%N)"
ms="$(((end_time-start_time)/1000000))"
if [ "${OK}" -eq 1 ]; then
  echo -en "${DIM}"
  echo "Done in ${ms} ms"
  echo -en "${COLOR_OFF}"
else
  echo -en "${RED}"
  echo "Failed in ${ms} ms"
  echo -en "${COLOR_OFF}"
fi

