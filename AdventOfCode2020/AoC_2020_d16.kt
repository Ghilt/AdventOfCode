import java.io.File

fun main() {
    val input = File("input/aoc_2020_d16.txt").readLines()

    val requirements = input.subList(0, input.indexOf("")).map { row ->
        val data = row.split(": ")[1].split(" ")
        val r = listOf(data[0], data[2]).flatMap { it.split("-") }.asInts()
        Req(row.substringBefore(":"), r[0]..r[1], r[2]..r[3])
    }
    val myTicket = input[input.indexOf("") + 2].split(",").asInts()
    val tickets = input.drop(input.indexOf("") + 5).map { it.split(",").asInts() }

    val errorRate = tickets.flatMap { it.filter { number -> !requirements.any { req -> number in req } } }.sum()
    println("Error rate: $errorRate")

    // Part 2:

    val validTickets = tickets.filter {
        it.all { number -> requirements.any { req -> number in req } }
    }.plus(listOf(myTicket))

    val labels = requirements.toMutableSet()
    var productOfDeparture = 1L
    repeat(requirements.size) {
        val matchingLabels = validTickets[0].indices.map { index ->
            index to validTickets
                .map { numbers -> labels.filter { numbers[index] in it }.toSet() }
                .reduce { acc, next -> acc intersect next }
        }

        val (isolatedColumn, label) = matchingLabels
            .first { it.second.size == 1 }
            .let { (index, reqs) -> index to reqs.first() }

        if (label.name.startsWith("dep")) productOfDeparture *= myTicket[isolatedColumn]
        labels.remove(label)
    }
    println("My ticket value: $productOfDeparture")
}

fun List<String>.asInts() = this.map { it.toInt() }

data class Req(val name: String, val range1: IntRange, val range2: IntRange) {
    operator fun contains(number: Int) = number in range1 || number in range2
}