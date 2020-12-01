from itertools import combinations

with open('input') as f:
    lines = list(map(int, f.readlines()))

def p1(lines):
    a, b = filter(lambda t: t[0] + t[1] == 2020, combinations(lines, 2)).__next__()
    return a * b

def p2(lines):
    a, b, c = filter(lambda t: t[0] + t[1] + t[2] == 2020, combinations(lines, 3)).__next__()
    return a * b * c

print(p1(lines))
print(p2(lines))
