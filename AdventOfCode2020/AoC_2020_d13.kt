import java.io.File

fun main() {
    val input = File("input/aoc_2020_d13.txt").readLines()

    val timeStamp = input[0].toInt()
    val bussInput = input[1].split(",")

    val availableBusses = bussInput.mapNotNull { it.toIntOrNull() }
    val waitTimes = availableBusses.map { it to (timeStamp / it + 1) * it - timeStamp }.sortedBy { it.second }
    val bestBusId = waitTimes[0].first * waitTimes[0].second

    println("Part 1: I arrive at $timeStamp, best bus id:  ${bestBusId}}")

    val busses = bussInput.mapIndexedNotNull { i: Int, v: String -> v.toIntOrNull()?.run { i to this } }
    val subsequentBusses = busses.drop(1)

    // This line generates a formula pastable into wolfram alpha which gives the answer
    val wolframAlphaString = "0 = ${busses.joinToString(" = ") { (delay, buss) -> "(t + $delay) mod $buss" }}"
    println(wolframAlphaString)


    // Too inefficient
    //    println("Result: ${generateSequence(0L) { it + 7 } .first { time ->
    //        subsequentBusses.all { (delay, buss) ->
    //            (time + delay) % buss == 0L
    //        }
    //    }}")

    // Fast enough:
    // Find the first matching timestamp one bus at a time
    val result: Pair<Long, Long> = subsequentBusses.fold(0L to busses[0].second.toLong()) { (earliestTimeForBussesThisFar, step), (delay, buss) ->
        val timeStampWithThisBusAsWell = generateSequence(earliestTimeForBussesThisFar) { it + step }
                .firstOrNull { time -> (time + delay) % buss == 0L } ?: earliestTimeForBussesThisFar

        // Step is a multiple of previous busses
        timeStampWithThisBusAsWell to step * buss
    }

    println("Result p2: $result")
}
