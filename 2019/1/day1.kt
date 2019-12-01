package moe.kageru.aoc

fun main() {
    val input = generateSequence(::readLine)
        .map { it.toLong() }
        .toList();
    println("Part 1: ${part1(input)}")
    println("Part 2: ${part2(input)}")
}

fun part1(numbers: List<Long>): Long = numbers.map(::cost).sum()

fun part2(numbers: List<Long>): Long = numbers.map { costRec(it, 0) }.sum()

fun cost(mass: Long): Long = (mass / 3) - 2

tailrec fun costRec(mass: Long, acc: Long): Long {
    val cost = cost(mass)
    return when {
        cost <= 0 -> acc
        else -> costRec(cost, acc + cost)
    }
}
