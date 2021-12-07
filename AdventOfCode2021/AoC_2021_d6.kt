import java.io.File

fun main() {
    val input = File("input/aoc_2021_d6.txt").readLines()[0]

    var fishes = input.split(",").map(String::toInt).groupBy { it }.mapValues { (_,v) -> v.size.toLong() }.toMutableMap()
    repeat(7) { fishes.putIfAbsent(it, 0L)}

    var infantFish = 0L
    var babyFish = 0L

    repeat(256) {
        val reachingAdult = babyFish
        babyFish = infantFish
        val day = it % 7
        infantFish = fishes[day]!!
        fishes[day] = fishes[day]!! + reachingAdult
    }

    println("Result: ${fishes.values.sum() + infantFish + babyFish}")
}
