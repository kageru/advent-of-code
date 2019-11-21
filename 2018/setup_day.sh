#!/bin/sh

today=$(date +%d)
mkdir "$today"
cd "$today"
# this assumes that your puzzle input is already in your clipboard
xsel -b > input

