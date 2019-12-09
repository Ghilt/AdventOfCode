import java.io.File

fun main(args: Array<String>) {
    val input = File("input/aoc_2019_d6.txt").readLines()

    val graph = input.map { it.split(")") }.groupingBy { it[1] }.fold("") { acc, e -> "${acc}${e[0]}" }
    val lengthMap = graph.mapValues { (_, v) -> dist(graph, v) }

    val p1 = lengthMap.flatMap { (_, v) -> listOf(v) }.reduce { acc, i -> acc + i }

    println("part 1: $p1")

    val pathMap = graph.mapValues { (_, v) -> paths(graph, v) }
    val start = pathMap["YOU"]!!
    val goal = pathMap["SAN"]!!

    val p2 = start.takeWhile { !goal.contains(it) }.size + goal.takeWhile { !start.contains(it) }.size

    println("part 2: $p2")
}

fun dist(data: Map<String, String>, v: String): Int = if (v == "COM") 1 else 1 + dist(data, data[v] ?: "")

fun paths(data: Map<String, String>, v: String): MutableList<String> = if (v == "COM") mutableListOf(v) else paths(data, data[v] ?: "").apply { add(0, v) }
