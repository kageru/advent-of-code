fun main() {
    val input = generateSequence(::readLine).map { it.split(")") }.toList()
    val parentToChildren = input.groupBy { it[0] }.mapValues { it.value.map { it[1] } }
    val rootNode = (parentToChildren.keys - parentToChildren.values.flatten().toSet()).first()
    println("Part 1: ${countOrbiters(parentToChildren, rootNode, 0)}")
    val childToParent = input.associateBy { it[1] }.mapValues { it.value[0] }
    val santaParents = getParents(childToParent, "SAN")
    val myParents = getParents(childToParent, "YOU")
    val commonParent = myParents.toSet().let { s -> santaParents.first { it in s } }
    println("Part 2: ${santaParents.indexOf(commonParent) + myParents.indexOf(commonParent)}")
}

fun <T> countOrbiters(graph: Map<T, List<T>>, key: T, acc: Int): Int =
    acc + (graph[key]?.map { countOrbiters(graph, it, acc+1) }?.sum() ?: 0)

fun <T> getParents(graph: Map<T, T>, key: T): List<T> =
    graph[key]?.let { curr -> listOf(curr) + getParents(graph, curr) } ?: emptyList()
