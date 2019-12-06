fun main() {
    val input = generateSequence(::readLine).map { it.split(")") }.groupBy { it[0] }.mapValues{ it.value.map { it[1] } }
    val rootNode = (input.keys - input.values.flatten().toSet()).first()
    println("Part 1: ${countOrbiters(input, rootNode, 0)}")
}

fun <T>countOrbiters(graph: Map<T, List<T>>, key: T, acc: Int): Int =
    acc + (graph[key]?.map { countOrbiters(graph, it, acc+1) }?.sum() ?: 0)
