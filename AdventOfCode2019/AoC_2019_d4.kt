import java.io.File

fun main(args: Array<String>) {
    val input = File("input/aoc_2019_d4.txt").readLines()[0]

    val startEnd = input.split("-").mapNotNull { it.toIntOrNull() }

    val p1 = (startEnd[0]..startEnd[1])
            .map { it.toString() }
            .filter { it.zipWithNext { a, b -> a <= b }.all { b -> b } }
            .filter { it.zipWithNext { a, b -> a == b }.any { b -> b } }

    println("part 1: ${p1.size}")

    val p2 = p1.filter {
        it.zipWithNext { a, b -> if (a == b) a else '_' }
                .joinToString("")
                .split("_")
                .any { c -> c.length == 1 }
    }

    println("part 2: ${p2.size}")
}

