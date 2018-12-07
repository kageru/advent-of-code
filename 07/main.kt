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
    for (entry in map) {
        println(entry)
    }
    while (map.size > 0) {
        val candidatesForNext = map.entries.map{a -> Pair(a.key, a.value.size)}//.map{e -> Pair(e.1, e.2.size()}
        println(candidatesForNext)
    }
}
