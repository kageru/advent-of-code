from collections import defaultdict
def cnt_orb(graph, key, acc): return acc + sum([cnt_orb(graph, k, acc+1) for k in graph[key]])
with open('input') as f: pairs = [l.strip().split(')') for l in f.readlines()]
parentToChildren = defaultdict(lambda: [])
for p in pairs: parentToChildren[p[0]].append(p[1])
print(cnt_orb(parentToChildren, 'COM', 0))
