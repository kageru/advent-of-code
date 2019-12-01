package moe.kageru.aoc

import java.io.File

fun main() {
  val lines = File("input").readLines().map { it.toInt() }
  println(lines.sum())
}
