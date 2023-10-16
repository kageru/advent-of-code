import strutils
import future
import nre


# 3-dimensional vector type with attributes for x, y, and z
# Mainly used for readability
type Vector = object
  x, y, z: int

# Are there proper constructors in nim?
# I guess this is... something.
proc newVector(x: int, y: int, z: int): Vector =
  var v: Vector
  v.x = x
  v.y = y
  v.z = z
  return v

# Manhattan Distance. Should be self-explanatory
method manhattan(this: Vector): int {.base.} =
  return abs(this.x) + abs(this.y) + abs(this.z)

proc countZeros(v: Vector): int =
  var z = 0
  for c in [v.x, v.y, v.z]:
    if c == 0:
      inc z
  return z

proc `$`(v: Vector): string =
  return $v.x & ", " & $v.y & ", " & $v.z

proc `<`(a: Vector, b:Vector): bool =
  if manhattan(a) == manhattan(b):
    return countZeros(a) < countZeros(b)
  return manhattan(a) < manhattan(b)

proc `>`(a: Vector, b:Vector): bool =
  if a == b:
    return countZeros(a) > countZeros(b)
  return manhattan(a) > manhattan(b)

proc `==`(a: Vector, b:Vector): bool =
  return a.x == b.x and a.y == b.y and a.z == b.z

proc `!=`(a: Vector, b:Vector): bool =
  return not (a == b)


type Particle = object
  number: int
  velocity: Vector
  acceleration: Vector
  position: Vector

proc newParticle(n: int, vel: Vector, acc: Vector, pos: Vector): Particle =
  var p: Particle
  p.number = n
  p.velocity = vel
  p.acceleration = acc
  p.position = pos
  return p

proc `>`(a: Particle, b: Particle): bool =
  if a.acceleration == b.acceleration:
    if a.velocity == b.velocity:
      return a.position > b.position
    return a.velocity > b.velocity
  return a.acceleration > b.acceleration

proc `<`(a: Particle, b: Particle): bool =
  if a.acceleration == b.acceleration:
    if a.velocity == b.velocity:
      return a.position < b.position
    return a.velocity < b.velocity
  return a.acceleration < b.acceleration

proc min(arr: seq[Particle]): Particle =
  var m = arr[0]
  for p in arr:
    if p < m:
      m = p
  return m



let input = lc[line | (line <- lines("input.txt")), string]
var particles: seq[Particle] = @[]

# p=<-1027,-979,-188>, v=<7,60,66>, a=<9,1,-7>
for i in 0..(len(input)-1):
  let line = input[i]
  let parsed = nre.match(line, re"p=.(?<px>-?\d+),(?<py>-?\d+),(?<pz>-?\d+).,\sv=.(?<vx>-?\d+),(?<vy>-?\d+),(?<vz>-?\d+).,\sa=.(?<ax>-?\d+),(?<ay>-?\d+),(?<az>-?\d+)")
  if parsed.isNone():
    echo("Could not parse ", line)
    continue
  let matched = parsed.get().captures
  # echo(i, ": ", line)
  particles.add(
      newParticle(i,
        newVector(parseInt(matched["vx"]), parseInt(matched["vy"]), parseInt(matched["vz"])),
        newVector(parseInt(matched["ax"]), parseInt(matched["ay"]), parseInt(matched["az"])),
        newVector(parseInt(matched["px"]), parseInt(matched["py"]), parseInt(matched["pz"]))
        )
     )

echo(min(particles).number)
