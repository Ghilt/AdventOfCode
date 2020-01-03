import java.io.File
import kotlin.math.pow

const val filePath = "D:/Files/Code/VisualStudio/AdventOfCode/AdventOfCode/AdventOfCode2019/"

fun main() {
    val input = File("${filePath}input/aoc_2019_d24.txt").readLines()

    var map: Map<Pair<Int, Int>, Boolean> = input.mapIndexed { y, row -> Pair(y, row.mapIndexed { x, char -> Pair(x, char) }) }
            .flatMap { indexedRow -> indexedRow.second.map { it.first to indexedRow.first to (it.second == '#') } }
            .toMap()

    map.print()

    val ratings = mutableSetOf<Int>()

    while (true) {
        val rating = map.getRating()
        val added = ratings.add(rating)
        if (!added) {
            println("The biodiversity rating which appears twice first is: $rating")
            break
        }
        map = nextTick(map)
    }
}

fun nextTick(currentState: Map<Pair<Int, Int>, Boolean>): Map<Pair<Int, Int>, Boolean> {
    return currentState.map { (key, state) ->
        val cx = key.first
        val cy = key.second
        var adjacent = currentState.isCellAlive(cx - 1, cy)
        adjacent += currentState.isCellAlive(cx + 1, cy)
        adjacent += currentState.isCellAlive(cx, cy - 1)
        adjacent += currentState.isCellAlive(cx, cy + 1)

        Pair(key, (adjacent == 1 && state) || (adjacent == 1 || adjacent == 2 && !state))
    }.toMap()
}

fun Map<Pair<Int, Int>, Boolean>.isCellAlive(x: Int, y: Int) = if (this[Pair(x, y)] == true) 1 else 0

fun Map<Pair<Int, Int>, Boolean>.getRating(): Int {
    return filterValues { it }.keys.fold(0) { acc, (x, y) -> acc + 2.0.pow(y * 5 + x).toInt() }
}

fun Map<Pair<Int, Int>, Boolean>.print() {
    val slate = List(5) { MutableList(5) { '.' } }
    filterValues { it }.keys.forEach { (x, y) -> slate[y][x] = '#' }
    print(slate.joinToString("\n", postfix = "\n\n") { it.joinToString(" ") { c -> "$c" } })
}