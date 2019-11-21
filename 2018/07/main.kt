import java.io.File
import java.io.InputStream

fun main(args: Array<String>) {
    val inputStream = File("input").inputStream()
    val inputLines = inputStream.bufferedReader().use { it.readText() }.split("\n")
    var map: HashMap<Char, HashSet<Char>> = HashMap()
    for (line in inputLines) {
        val step = line[36]
        val dep = line[5]

        // Make sure values that are only referenced as dependencies
        // (i. e. the first step in our chain) are also added to the set.
        map.getOrPut(dep, { HashSet<Char>() })

        val currentDeps = map.getOrPut(step, { HashSet<Char>() })
        currentDeps.add(dep)
    }
    var output = mutableListOf<Char>()
    while (map.size > 0) {
        // The keyset in Kotlin is already sorted, so we can just take the first value
        val next = map.entries.map{ e -> Pair(e.key, e.value.size) }.filter{ e -> e.second == 0 }.map{ e -> e.first }[0]
        map.remove(next)
        map.values.map{ v -> v.remove(next) }
        output.add(next)
    }
    println(output.joinToString(""))
}
