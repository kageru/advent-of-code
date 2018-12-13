package aoc.thirteen

import aoc.thirteen.Field
import aoc.thirteen.Turn
import aoc.thirteen.Train
import java.io.File
import java.io.InputStream

class Main() {

    companion object {
    public var field: MutableList<MutableList<Field>> = mutableListOf<MutableList<Field>>()
    var noCrash = true

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
            0 -> t.y += 1
            1 -> t.x += 1
            2 -> t.y -= 1
            3 -> t.x -= 1
            else -> throw IllegalArgumentException("this shouldn’t happen either selphyDerp")
        }
        val current = field[t.x][t.y]
        when (current) {
            Field.VERTICAL, Field.HORIZONTAL -> {}
            Field.TOP_LEFT -> t.direction = t.direction xor 1
            Field.TOP_RIGHT -> t.direction = t.direction xor 3
            Field.INTERSECTION -> t.crossIntersection()
            Field.EMPTY -> IllegalStateException("I shouldn’t be here")
        }
        return t
    }

    @JvmStatic
    fun main(args: Array<String>) {
        val inputStream = File("input").inputStream()
        val inputLines = inputStream.bufferedReader().use { it.readText() }.split("\n")
        val TRAINS = charArrayOf('<', '>', '^', 'v')
        
        var trains = mutableListOf<Train>()

        for ((x, line) in inputLines.withIndex()) {
            var fields = mutableListOf<Field>()
            for ((y, char) in line.toCharArray().withIndex()) {
                if (char in TRAINS) {
                    trains.add(parseTrain(char, x, y))
                }
                fields.add(parseField(char))
            }
            field.add(fields)
        }
        var positions = HashSet<Pair<Int, Int>>(trains.size)
        positions.addAll(trains.map{ t -> Pair(t.x, t.y) })
        while (positions.size == trains.size) {
            trains.forEach{ t -> moveTrain(t) }
            positions.clear()
            positions.addAll(trains.map{ t -> Pair(t.x, t.y) })
        }
        println(trains)
    }
    }
}
