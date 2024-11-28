from math import prod
with open('day2') as f:
    lines = [list(map(int, l.split('x'))) for l in f.readlines()]

def slack(a, b, c):
    return 2 * (a * b + b * c + a * c) + min(a * b, b * c, a * c)

def ribbon(l):
    return prod(l) + sum(sorted(l)[:2]) * 2

print(f'Part 1: {sum([slack(*l) for l in lines])}')
print(f'Part 2: {sum(map(ribbon, lines))}')
