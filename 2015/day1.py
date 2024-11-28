import itertools

with open('day1') as f:
    line = f.read().strip()

print(f'Part 1: {line.count("(") - line.count(")")}')
print(f'Part 2: {list(itertools.accumulate(map(lambda s: 1 if s == "(" else -1, line))).index(-1) + 1}')
