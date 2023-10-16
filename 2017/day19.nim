import sequtils
import future
import nre except toSeq



type
  Direction = enum
    up = 0, left = 1, down = 2, right = 3
  Grid = seq[seq[char]]

const directions = [up, left, down, right]

proc indexOf(hay: seq[char], needle: char): int =
  # It almost seems weird that this doesn’t already exist...
  # It probably does, and I just missed it. Whatever, no error handling because :lul:
  var i = 0
  while hay[i] != needle:
    i += 1
  return i


proc gridGet(g: Grid, p: array[2, int]): char =
  return g[p[0]][p[1]]


proc changePosition(current: array[2, int], dir: Direction): array[2, int] =
  if dir == up:
    return [current[0]-1, current[1]]
  if dir == left:
    return [current[0], current[1]-1]
  if dir == down:
    return [current[0]+1, current[1]]
  if dir == right:
    return [current[0], current[1]+1]


proc oppositeDirection(d: Direction): Direction =
  # the better solution would be something like
  # mod(getEnumOrdinal(direction, $direction) + 2, 4)
  # but that doesn’t quite work because I’m bad at this whole thing
  case d
    of up:     return down
    of left:   return right
    of down:   return up
    of right:  return left
    else:      discard

proc changeDirection(g: Grid, pos: array[2, int], direction: Direction): Direction =
  for d in directions:
    if d != oppositeDirection(direction) and gridGet(g, changePosition(pos, d)) != ' ':
      return d


let grid = lc[toSeq(line.items()) | (line <- lines("input.txt")), seq[char]]

var position = [0, indexOf(grid[0], '|')]
var direction = down
var done = false
var visited = ""
var newchar: char
var steps = 1

while not done:
  #echo position[0], ", ", position[1]
  #echo direction
  var newpos = changePosition(position, direction)
  newchar = gridGet(grid, newpos)
  #echo(newchar)
  if newchar in ['|', '-']:
    inc steps
  elif newchar == '+':
    inc steps
    direction = changeDirection(grid, newpos, direction)
  elif nre.contains($newchar, re"[A-Z]"):
    inc steps
    visited &= newchar
  elif newchar == ' ':
    done = true
  position = newpos

echo(visited)
echo(steps)
