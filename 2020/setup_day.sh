#!/bin/sh

today=$(date +%d)
mkdir "$today"
cd "$today"
cargo init --name "day$today"
echo 'Initialized cargo project'

# this assumes that your puzzle input is already in your clipboard
xsel -b > input
# add trailing newline if necessary
sed -i -e '$a\' input
