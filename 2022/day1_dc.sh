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

_='
Commented version of the code above:

# Macro `r` which sums a group of values on the stack
[
  # Add the top 2 values of the stack
  +
  # If the stack size (z) is not equal to `ld` (the number of elves processed so far +1), recurse.
  # That means we’ll keep reducing the stack until all values of this batch are processed.
  zld!=r
]sr

# Macro `g`, which prepares everything to process an elf
[
  # Number of elves processed will be in `d`. Load that, add 1 to it, and store it again.
  # (will load 0 on first iteration)
  ld1+sd 
  # Push 0 to the stack.
  # This makes stack reduction above easier because we can assume that every elf
  # has at least two values (one of them will be this 0)
  0
  # Call `r` from above to sum all values in this group.
  lrx
]sg

# Macro `s` which swaps the top 2 elements of the stack
[r]ss

# Macro `f` which reduces the stack until only the biggest value is left
[
  # Duplicate the top two values on the stack.
  # e.g. [1, 2, 3] would become [1, 2, 3, 2, 3]
  # The comparison right after this pops the top two values,
  # so we have to duplicate them to use them after.
  sasblalblalb
  # If the top value is greater than the one below, call `s` to switch them.
  >s              
  # Pop the top value (now the smaller of the two) and store it in `x`
  # (there’s no way to just pop, so we instead store
  # to a register that is overwritten in each iteration)
  sx              
  # If stack size is not 1, recurse.
  z1!=f
]sf

# This puts the inputs into our dc file, replacing blank lines with `lgx`.
# Since blank lines are meaningless in dc,
# we have to insert something between the blocks that will separate them.
# `lgx` will load and execute the macro `x`,
# which increments the total number of elves and sums the calories of that elf.
cat inputs/day01 | sed "s/^$/lgx/" >> day1.tmp

# Add one last `lgx` to batch up the last elf, then execute macro `f` which finds the biggest value on the stack, then print that.
echo "lgxlfxp" >> day1.tmp
'
