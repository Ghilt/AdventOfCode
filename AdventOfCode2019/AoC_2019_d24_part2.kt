import java.io.File

const val filePath = "D:/Files/Code/VisualStudio/AdventOfCode/AdventOfCode/AdventOfCode2019/"

fun main() {
    val input = File("${filePath}input/aoc_2019_d24.txt").readLines()

    val map: Map<Pair<Int, Int>, Boolean> = input.mapIndexed { y, row -> Pair(y, row.mapIndexed { x, char -> Pair(x, char) }) }
            .flatMap { indexedRow -> indexedRow.second.map { it.first to indexedRow.first to (it.second == '#') } }
            .toMap()

    var structure = listOf(createEmpty(), map.filterRemoveCenter(), createEmpty())

    repeat(200) { structure = nextTick(structure) }

    structure.drop(structure.size / 2).take(5).forEach { it.print() }

    val insects = structure.flatMap { it.toList() }.filter { (_, alive) -> alive }.count()
    println("There are $insects insectoids present")
}

private fun Map<Pair<Int, Int>, Boolean>.filterRemoveCenter() = filterNot { (k, _) -> k.first == 2 && k.second == 2 }

fun nextTick(currentState: List<Map<Pair<Int, Int>, Boolean>>): List<Map<Pair<Int, Int>, Boolean>> {
    val newState = currentState.toMutableList()
    // Spawn new empty boards near the edges of the recursive board if needed
    if (currentState[0].any { (_, v) -> v } || currentState[1].any { (_, v) -> v }) {
        newState.add(0, createEmpty())
    }

    if (currentState.last().any { (_, v) -> v } || currentState[currentState.size - 2].any { (_, v) -> v }) {
        newState.add(createEmpty())
    }

    return newState.mapIndexed { i, level ->
        if (i != 0 && i != newState.size - 1) {
            nextTick(newState[i - 1], newState[i + 1], level)
        } else {
            newState[i]
        }
    }.toList()
}

fun createEmpty() = (0..4).flatMap { x -> (0..4).map { y -> Pair(Pair(x, y), false) } }.toMap().filterRemoveCenter()

fun nextTick(upLevel: Map<Pair<Int, Int>, Boolean>,
             downLevel: Map<Pair<Int, Int>, Boolean>,
             currentState: Map<Pair<Int, Int>, Boolean>): Map<Pair<Int, Int>, Boolean> {
    return currentState.map { (key, state) ->
        val cx = key.first
        val cy = key.second
        var adjacent = currentState.isCellAlive(cx - 1, cy)
        adjacent += currentState.isCellAlive(cx + 1, cy)
        adjacent += currentState.isCellAlive(cx, cy - 1)
        adjacent += currentState.isCellAlive(cx, cy + 1)
        adjacent += getAdjacentOneLevelUp(cx, cy, upLevel)
        adjacent += getAdjacentOneLevelDown(cx, cy, downLevel)

        Pair(key, (adjacent == 1 && state) || (adjacent == 1 || adjacent == 2 && !state))
    }.toMap()
}

fun getAdjacentOneLevelUp(x: Int, y: Int, upLevel: Map<Pair<Int, Int>, Boolean>): Int {
    var adjacent = 0
    if (y == 0) adjacent += upLevel.isCellAlive(2, 1)
    if (y == 4) adjacent += upLevel.isCellAlive(2, 3)
    if (x == 0) adjacent += upLevel.isCellAlive(1, 2)
    if (x == 4) adjacent += upLevel.isCellAlive(3, 2)
    return adjacent
}

fun getAdjacentOneLevelDown(x: Int, y: Int, downLevel: Map<Pair<Int, Int>, Boolean>): Int {
    return when {
        x == 2 && y == 1 -> downLevel.isCellsAlive(0..4, 0..0)
        x == 2 && y == 3 -> downLevel.isCellsAlive(0..4, 4..4)
        x == 1 && y == 2 -> downLevel.isCellsAlive(0..0, 0..4)
        x == 3 && y == 2 -> downLevel.isCellsAlive(4..4, 0..4)
        else -> 0
    }
}

fun Map<Pair<Int, Int>, Boolean>.isCellAlive(x: Int, y: Int) = if (this[Pair(x, y)] == true) 1 else 0

fun Map<Pair<Int, Int>, Boolean>.isCellsAlive(xRange: IntRange, yRange: IntRange): Int {
    return filterKeys { (x, y) -> y in yRange && x in xRange }.filterValues { it }.count()
}

fun Map<Pair<Int, Int>, Boolean>.print() {
    val slate = List(5) { MutableList(5) { '.' } }
    slate[2][2] = '?'
    filterValues { it }.keys.forEach { (x, y) -> slate[y][x] = '#' }
    print(slate.joinToString("\n", postfix = "\n\n") { it.joinToString(" ") { c -> "$c" } })
}
