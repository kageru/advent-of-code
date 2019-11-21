import java.io.File
import java.io.InputStream

fun main(args: Array<String>) {
    val inputStream: InputStream = File("input").inputStream()
    val inputLines = inputStream.bufferedReader().use { it.readText() }.split("\n");
    var twos = 0
    var threes = 0
    for (line in inputLines) {
        var map:HashMap<Char, Int> = HashMap<Char, Int>(inputLines.size)
        for (char in line) {
            val current = map.get(char)
            map.set(char, if (current == null) 1 else current + 1)
        }
        if (2 in map.values) {
            twos++
        }
        if (3 in map.values) {
            threes++
        }
    }
    println("Doubles: " + twos)
    println("Triples: " + threes)
    println(twos * threes)
}
