from collections import defaultdict, deque


def day9(players, last):
    marbles = deque([0])
    player = 1
    scores = defaultdict(lambda: 0)
    for m in range(1, last+1):
        if m % 23 == 0:
            marbles.rotate(7)
            scores[player] += m + marbles.pop()
            marbles.rotate(-1)
        else:
            marbles.rotate(-1)
            marbles.append(m)
        player = (player + 1) % players
    print(max(scores.values()))


if __name__ == '__main__':
    day9(404, 71852)
