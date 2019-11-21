#!/usr/bin/env python3

from collections import namedtuple, UserDict

Pot = namedtuple('Pot', ['id', 'plant'])
Rule = namedtuple('Rule', ['inp', 'out'])
newgen = None

class PlantDict(UserDict):
    def __init__(self, cb, initialdata):
        super().__init__(initialdata)
        self._cb = cb


    def __missing__(self, key):
        val = self._cb(key)
        self[key] = val
        return val


def default_pot(i):
    return Pot(i, False)


def parse_rule(rule):
    return Rule([c == '#' for c in rule[:5]], '#' in rule.split()[-1])
    

def print_pots(pots):
    print(''.join(['#' if pot.plant else '.' for pot in pots]))


def main():
    # I’m aware that I’m overwriting a builtin here
    input = []
    with open('input') as f:
        input = f.readlines()
    new_gen = PlantDict(default_pot, enumerate([Pot(i, c == '#') for i, c in enumerate(input[0].split(': ')[1])]))
    print_pots(new_gen.values())
    rules = [parse_rule(r) for r in input[2:]]
    before = after = 0
    for i in range(1000):
        before = after
        old_gen = new_gen.copy()
        ids = new_gen.keys()
        for i in range(min(ids) - 2, max(ids) + 3): 
            surroundings = [p.plant for p in [old_gen[x] for x in range(i-2, i+3)]]
            for rule in rules:
                if surroundings == rule.inp:
                    new_gen[i] = Pot(i, rule.out)
                    break
        #print_pots(new_gen.values())
    
        after = sum([pot.id for pot in new_gen.values() if pot.plant])
        diff = after - before

    print(diff, before)
    print(before + diff * (50000000000 - 999))


if __name__ == '__main__':
    main()
