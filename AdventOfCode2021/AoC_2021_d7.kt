import kotlin.math.abs
import kotlin.collections.minBy
import java.io.File

fun main() {
    val input = File("input/aoc_2021_d7.txt").readLines()[0].split(",").map(String::toInt)

    val distances = input.groupBy { it }.mapValues { 0 to 0 }.toMutableMap()
    repeat(input.maxOrNull() ?: 0) { distances.putIfAbsent(it, 0 to 0)}

    input.forEach { startingPos ->
        distances.keys.forEach {
            val distance = abs(it - startingPos)
            distances[it] = (distances[it]!!.first + distance to distances[it]!!.second + distance * (distance + 1) / 2)
        }
    }

    println("Result part1: ${distances.minByOrNull {it.value.first}?.value?.first}")
    println("Result part2: ${distances.minByOrNull {it.value.second}?.value?.second}")
}
