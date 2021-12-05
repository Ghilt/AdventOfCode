import java.io.File
import java.lang.Integer.max
import kotlin.math.abs
import kotlin.math.sign

data class Pos(val x: Int, val y: Int)

fun main() {
    val input = File("input/aoc_2021_d5.txt").readLines()

    val parsed = input.map { row ->
        val cords = row.split(" -> ").map { it.split(",").map(String::toInt) }
        (Pos(cords[0][0], cords[0][1])) to (Pos(cords[1][0], cords[1][1]))
    }

    val task1Input = parsed.filter { it.first.x == it.second.x || it.first.y == it.second.y }
    val task2Input = parsed

    val grid1 = paintLines(task1Input)
    val grid2 = paintLines(task2Input)

    /* println("Result: \n${grid1.joinToString("\n")}") */
    println("Result: ${grid1.flatten().count { it > 1 }}")
    println("Result: ${grid2.flatten().count { it > 1 }}")
}

private fun paintLines(lines: List<Pair<Pos, Pos>>): MutableList<MutableList<Int>> {
    val size = 1 + lines.flatMap { listOf(it.first.x, it.first.y, it.second.x, it.second.y) }.sorted().last()
    val grid = MutableList(size) { MutableList(size) { 0 } }
    lines.forEach { grid.paintLine(it.first, it.second) }
    return grid
}

fun MutableList<MutableList<Int>>.paintLine(p1: Pos, p2: Pos) {
    val xDiff = p2.x - p1.x  /* the below code is the worst in the universe. I didn't know that part2 was going to only care about 45degree angles */
    val yDiff = p2.y - p1.y
    val gcd = gcd(xDiff, yDiff)
    if (gcd == 0) {
        paintLine(p2, p1)
        return
    }
    val xStep = abs(xDiff / gcd)
    val yStep = abs(yDiff / gcd)   /* Formatted so that it is as unreadable to hide its horribleness */
    val points = max(abs(xDiff / xStep.let { if (it == 0) 1 else it }), abs(yDiff / yStep.let { if (it == 0) 1 else it }))
    for (touchdowns in 0..points) {this[p1.y + touchdowns * yStep * yDiff.sign][p1.x + touchdowns * xStep * xDiff.sign]++}
}

private fun gcd(a1: Int, b1: Int): Int {
    var a = a1
    var b = b1
    while (b > 0) {
        val temp = b
        b = a % b
        a = temp
    }
    return a
}
