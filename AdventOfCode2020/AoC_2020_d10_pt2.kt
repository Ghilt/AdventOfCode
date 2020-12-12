import java.io.File

val memo = mutableMapOf<Int, Long>()

fun main() {
    val input = File("input/aoc_2020_d10.txt").readLines().map { it.toInt() }.sorted()

    val with0AndDevice = input.toMutableList()
    with0AndDevice.add(0, 0)
    with0AndDevice.add(input.last() + 3)

    println(solve(with0AndDevice, 0))
}

fun solve(input: List<Int>, adapterIndex: Int): Long {
    if (adapterIndex == input.size - 1) return 1
    return memo[adapterIndex] ?: calculate(input, adapterIndex)
}

fun calculate(input: List<Int>, adapterIndex: Int): Long {
    var paths: Long = 0;
    for (i in (adapterIndex + 1) until input.size) {
        if (input[i] - input[adapterIndex] <= 3) paths += solve(input, i)
    }
    memo[adapterIndex] = paths
    return paths
}
