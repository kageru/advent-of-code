#!/bin/sh

awk -f day4.awk input | rg --pcre2 '(.)\1' | wc -l | awk '{printf("Part 1: %s\n", $1);}'
