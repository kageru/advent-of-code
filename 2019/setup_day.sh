#!/bin/sh

today=$(date +%d)
echo 'Select language for today. (r)ust, (k)otlin:'
read lang

if [[ "$lang" == "r" ]]; then
    mkdir "$today"
    cd "$today"
    cargo init --name "day$today"
    echo 'Initialized cargo project'
elif [[ "$lang" == "k" ]]; then
    cp -r kt "$today"
    cd "$today"
    sed -i "s/DayXX/Day$today/" build.gradle.kts
    mv "src/main/kotlin/moe/kageru/aoc/DayXX.kt" "src/main/kotlin/moe/kageru/aoc/Day$today.kt"
    echo 'Initialized gradle project from skeleton'
else
    echo "Unknown language $lang"
    exit 1
fi

# this assumes that your puzzle input is already in your clipboard
xsel -b > input
# add trailing newline if necessary
sed -i -e '$a\' input
