import java.io.File
import kotlin.math.abs

fun main() {
    val input = File("input/aoc_2021_d13.txt").readLines()

    val lines = input.partition { it.startsWith("fold") }
    val folds = lines.first.map { f -> f.toPair("=", String::last, String::toInt) }
    val dots = lines.second.filter { it != "" }.map { coordinate -> coordinate.toPair(",", String::toInt, String::toInt) }

    val foldedUp = folds.foldIndexed(dots) { i, tp, (dimen, v) ->
        val foldIt: (Pair<Int, Int>) -> Pair<Int, Int> =
            if (dimen == 'x') {{ if (it.first > v) v - abs(v - it.first) to it.second else it }}
            else {{ if (it.second > v) it.first to v - abs(v - it.second) else it }}
        tp.map(foldIt).also { if (i == 0) println("Result pt1: ${it.distinct().size}") /* Hey, it's side effect showing result pt1, very cheeky */ }
    }

    for (y in 0..foldedUp.maxOf { it.second }) {
        for (x in 0..foldedUp.maxOf { it.first }) {
            print(if (foldedUp.contains(x to y)) "█" else ".")
        }
        println()
    }
}

fun <A, B> String.toPair(delimiter: String, firstConverter: (String) -> A, secondConverter: (String) -> B): Pair<A, B> {
    val cleaved = split(delimiter)
    return firstConverter(cleaved[0]) to secondConverter(cleaved[1])
}