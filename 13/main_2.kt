package aoc.thirteen

import aoc.thirteen.Field
import aoc.thirteen.Turn
import aoc.thirteen.Train
import java.io.File
import java.io.InputStream

class Main() {

    companion object {
    public var field: MutableList<MutableList<Field>> = mutableListOf<MutableList<Field>>()

    fun parseField(c: Char): Field {
        return when (c) {
            '|', 'v', '^' -> Field.VERTICAL
            '-', '>', '<' -> Field.HORIZONTAL
            '/' -> Field.TOP_LEFT
            '\\' -> Field.TOP_RIGHT
            '+' -> Field.INTERSECTION
            ' ' -> Field.EMPTY
            else -> throw IllegalArgumentException("can’t parse field " + c)
        }
    }

    fun cleanUpAfterCrash(trains: MutableList<Train>): Pair<Int, Int> {
        for ((i, train) in trains.filter{ it.alive }.withIndex()) {
            for ((j, t2) in trains.filter{ it.alive }.withIndex()) {
                if (train != t2 && train.x == t2.x && train.y == t2.y) {
                    return Pair(i, j)
                }
            }
        }
        return Pair(-1, -1)
    }

    fun parseTrain(c: Char, x: Int, y: Int): Train {
        val dir = when (c) {
            '^' -> 0
            '>' -> 1
            'v' -> 2
            '<' -> 3
            else -> throw IllegalArgumentException("this shouldn’t happen rooDerp")
        }
        return Train(dir, x, y)
    }

    fun moveTrain(t: Train): Train {
        when (t.direction) {
            0 -> t.y -= 1
            1 -> t.x += 1
            2 -> t.y += 1
            3 -> t.x -= 1
            else -> throw IllegalArgumentException("this shouldn’t happen either selphyDerp")
        }
        val current = field[t.y][t.x]
        when (current) {
            Field.VERTICAL, Field.HORIZONTAL -> {}
            Field.TOP_LEFT -> t.direction = t.direction xor 1
            Field.TOP_RIGHT -> t.direction = t.direction xor 3
            Field.INTERSECTION -> t.crossIntersection()
            Field.EMPTY -> throw IllegalStateException("I shouldn’t be here")
        }
        return t
    }

    @JvmStatic
    fun main(args: Array<String>) {
        val inputStream = File("input").inputStream()
        val inputLines = inputStream.bufferedReader().use { it.readText() }.split("\n")
        val TRAINS = charArrayOf('<', '>', '^', 'v')
        
        var trains = mutableListOf<Train>()

        for ((y, line) in inputLines.withIndex()) {
            var fields = mutableListOf<Field>()
            for ((x, char) in line.toCharArray().withIndex()) {
                if (char in TRAINS) {
                    val newTrain = parseTrain(char, x, y)
                    trains.add(newTrain)
                }
                fields.add(parseField(char))
            }
            field.add(fields)
        }
        var positions = HashSet<Pair<Int, Int>>(trains.size)
        positions.addAll(trains.map{ t -> Pair(t.x, t.y) })
        var current = 0
        while (trains.filter{ it.alive }.size > 1) {
            while (positions.size == trains.filter{ it.alive }.size) {
                if (!trains[current].alive)
                    continue
                trains[current] = moveTrain(trains[current])
                positions.clear()
                positions.addAll(trains.filter{ it.alive }.map{ t -> Pair(t.x, t.y) })
                current = (current + 1) % trains.size
            }
            val remove = cleanUpAfterCrash(trains)
            println("removing $remove")
            trains[remove.first].alive = false
            trains[remove.second].alive = false

            positions.clear()
            positions.addAll(trains.filter{ it.alive }.map{ t -> Pair(t.x, t.y) })
        }
    }
    }
}
