import java.io.File

typealias Caves = List<String>
typealias Paths = List<Caves>

fun main() {
    val input = File("input/aoc_2021_d12.txt").readLines().map { it.split("-") }
    val caveSystem = parse(input)
    println("Result p1: ${getPaths(listOf(listOf("start")), caveSystem, Caves::oneCave).size}")
    println("Result p2: ${getPaths(listOf(listOf("start")), caveSystem, Caves::twoCave).size}")
}

fun getPaths(paths: Paths, caveMap: Map<String, Caves>, limit: (Caves) -> Boolean): Paths =
    if (paths.all { it.last() == "end" }) paths 
    else getPaths(paths.flatMap { caveMap[it.last()]?.map { newCave -> it + newCave }?.filter(limit) ?: listOf(it) }, caveMap, limit)

fun Caves.oneCave() = filter { it.all(Char::isLowerCase) }.run { distinct().size == size }
fun Caves.twoCave() = filter { it.all(Char::isLowerCase) }.run { distinct().size >= size - 1 }

fun parse(input: Paths) = (input.filterNot { it[1] == "start" } + input.filterNot { it[0] == "start" }.map(Caves::reversed))
    .groupBy(Caves::first)) { it[1] }.filterKeys { it != "end" }

