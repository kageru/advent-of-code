#!/bin/sh
echo '
[+zld!=r]sr
[ld1+sd0lrx]sg
[r]ss
[sasblalblalb>ssxz1!=f]sf' > day1.tmp
cat inputs/day01 | sed 's/^$/lgx/' >> day1.tmp
echo 'lgxlfxp' >> day1.tmp
dc -f day1.tmp
rm day1.tmp
