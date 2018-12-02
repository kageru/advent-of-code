import java.io.File
import java.io.InputStream

fun countDifferences(s1: String, s2: String): Int {
    var diffs = 0
    for (pair in s1 zip s2) {
        if (pair.first != pair.second) {
            diffs++
        }
    }
    return diffs
}

fun main(args: Array<String>) {
    val inputStream: InputStream = File("input").inputStream()
    val inputLines = inputStream.bufferedReader().use { it.readText() }.split("\n");
    for (line1 in inputLines) {
        for (line2 in inputLines) {
            if (countDifferences(line1, line2) == 1) {
                println(line1)
                println(line2)
                return
            }
        }
    }
}
