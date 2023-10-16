import tables
import nre
import strutils


let pattern = re"(?<target>\w+)\s(?<opcode>(inc|dec))\s(?<value>-?\d+)\sif\s(?<comptarget>\w+)\s(?<compop>(<|>|==|!=|<=|>=))\s(?<comp2>-?\d+)"

var registers = initTable[string, int]();


proc parseLine(li: string): RegexMatch =
  let found = nre.find(li, pattern)
  if found.isNone:
    echo("mismatch")
    return
  return found.get()


proc compare(target: string, val2: int, op: string): bool =
  # text parsing done right ヽ( ﾟヮ・)ノ
  let val1 = registers[target]
  case op
    of "<":
      return val1 < val2;
    of ">":
      return val1 > val2;
    of "==":
      return val1 == val2;
    of "!=":
      return val1 != val2;
    of "<=":
      return val1 <= val2;
    of ">=":
      return val1 >= val2;
    else: discard


proc changeRegister(r: string, op: string, val: int): void =
  # only ‘inc’ and ‘dec’ are possible
  if op == "inc":
    registers[r] += val
  else:
    registers[r] -= val


proc executeLine(ins: Table): void =
  # set registers to 0 if they haven’t been initialized
  if not registers.hasKey(ins["comptarget"]):
    registers[ins["comptarget"]] = 0
  if not registers.hasKey(ins["target"]):
    registers[ins["target"]] = 0
  # do math... or don’t
  if compare(ins["comptarget"], parseInt(ins["comp2"]), ins["compop"]):
    changeRegister(ins["target"], ins["opcode"], parseInt(ins["value"]))


for line in lines "day8.txt":
  let parsed = parseLine(line)
  executeLine(parsed.captures.toTable())


# returning a list like with python’s dict.values doesn’t seem to work.
# is there a way to convert iterables to arrays?
var maximum = 0
for v in values(registers):
  if v > maximum:
    maximum = v

echo(maximum)
