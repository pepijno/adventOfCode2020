#!/usr/bin/env bash

stack build
echo "" > results
for filename in app/Day*.hs; do
	day="day${filename//[^0-9]/}"
	echo "$day"
	echo "$day:" >> results
	stack run $day >> results
done
