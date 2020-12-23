import java.io.File
import java.lang.Integer.min
import kotlin.math.max

typealias Deck = List<Int>

fun main() {
    val input = File("input/aoc_2020_d22.txt").readLines().filter { !it.startsWith("P") }
    val deck1 = input.takeWhile { it != "" }.map { it.toInt() }
    val deck2 = input.takeLastWhile { it != "" }.map { it.toInt() }
    val d1 = deck1.toMutableList()
    val d2 = deck2.toMutableList()

    while (d1.isNotEmpty() && d2.isNotEmpty()) {
        val winner = if (d1[0] > d2[0]) d1 else d2
        val high = max(d1[0], d2[0])
        val low = min(d1[0], d2[0])
        d1.removeAt(0)
        d2.removeAt(0)
        winner.add(winner.size, high)
        winner.add(winner.size, low)
    }

    val winner = if (d1.isEmpty()) d2 else d1
    val result = winner.reversed().foldIndexed(0) { i, acc, e -> acc + e * (i + 1)}

    println(result)

    val result2 = playGame(deck1, deck2).second.reversed().foldIndexed(0) { i, acc, e -> acc + e * (i + 1)}

    println(result2)
}

fun playGame(deck1: Deck, deck2: Deck): Pair<Boolean, Deck> {
    val d1 = deck1.toMutableList()
    val d2 = deck2.toMutableList()

    val states = mutableSetOf<Pair<Deck, Deck>>()

    while (d1.isNotEmpty() && d2.isNotEmpty()) {
        if (d1.toList() to d2.toList() in states) {
            return true to d1
        }
        states.add(d1.toList() to d2.toList())

        var pl1Win: Boolean
        val (winCard, loseCard) = if (d1[0] < d1.size && d2[0] < d2.size) {
            val (winner, _) = playGame(d1.drop(1).take(d1[0]), d2.drop(1).take(d2[0]))
            pl1Win = winner
            if (winner) d1[0] to d2[0] else d2[0] to d1[0]
        } else {
            pl1Win = d1[0] > d2[0]
            val high = max(d1[0], d2[0])
            val low = min(d1[0], d2[0])
            high to low
        }

        val winner = if (pl1Win) d1 else d2
        d1.removeAt(0)
        d2.removeAt(0)
        winner.add(winner.size, winCard)
        winner.add(winner.size, loseCard)

    }

    return d1.isNotEmpty() to if (d1.isNotEmpty()) d1 else d2
}