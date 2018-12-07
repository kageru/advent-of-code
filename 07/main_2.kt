import java.io.File
import java.io.InputStream
import kotlin.math.max

fun main(args: Array<String>) {
    val numWorkers = 5

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
    var workers = MutableList<Pair<Char, Int>>(numWorkers, { Pair( ' ', 0 ) })

    var timer = 0

    // While there are still objects to be processed or at least one worker is still working
    while (map.size > 0 || true in workers.map{ w -> w.second > 0 }) {
        // Only remove the element from dependencies once it’s been worked on
        for (worker in workers) {
            if (worker.second == 0) {
                map.values.map{ v -> v.remove(worker.first) }
            }
        }

        while (map.size > 0 && 0 in workers.map{ w -> w.second }) {
            // The keyset in Kotlin is already sorted, so we can just take the first value
            val candidates = map.entries.map{ e -> Pair(e.key, e.value.size) }.filter{ e -> e.second == 0 }.map{ e -> e.first }
            if (candidates.size == 0) { 
                break
            }
            val next = candidates[0]

            val currentWorker = workers.indexOf(workers.find{ w -> w.second == 0 })
            // The char 'A' is 65, but we want 60 + its position in the alphabet
            workers[currentWorker] = Pair(next, next.toInt() - 4)
            map.remove(next)
        }
        timer++
        workers = workers.map{ w -> Pair(w.first, max(0, w.second - 1)) }.toMutableList()
    }
    println(timer)
}
