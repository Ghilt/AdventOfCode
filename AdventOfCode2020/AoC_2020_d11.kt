import java.io.File


fun main() {
    val input = File("input/aoc_2020_d11.txt").readLines()
    val length = input.size

    val grid = input.foldIndexed(listOf<Seat>()) { rowIndex, acc, row ->
        row.foldIndexed(acc) { columnIndex, rc, seat ->
            if (seat == '.') {
                rc
            } else {
                rc.plus(Seat(false, rowIndex, columnIndex))
            }
        }
    }.associateBy { seat -> seat.x to seat.y }

    val occupiedSeats = countSeatsWhenStable(4, grid)

    println("Seats filled pt1: $occupiedSeats")

    val watchfulSeats = grid.map { (_, seat) -> seat.watchAround(length, grid) }.associateBy { seat -> seat.x to seat.y }
    val occupiedSeatsPt2 = countSeatsWhenStable(5, watchfulSeats)

    println("Seats filled pt2: $occupiedSeatsPt2")
}

private fun countSeatsWhenStable(rules: Int, grid: Map<Pair<Int, Int>, Seat>): Int {
    var oldGrid = grid
    var newGrid = doPass(rules, grid)
    while (oldGrid != newGrid) {
        oldGrid = newGrid
        newGrid = doPass(rules, newGrid)
    }
    return newGrid.values.count { it.filled }
}

private fun Seat.watchAround(length : Int,grid: Map<Pair<Int, Int>, Seat>): Seat {
    val posToCheck = mutableListOf<Pair<Int, Int>>()
    for (x in -1..1) {
        for (y in -1..1) {
            val toCheck = generateSequence((this.x + x) to (this.y + y)) { (nX, nY) -> nX + x to nY + y }
                    .take(length)
                    .firstOrNull { c -> grid[c] != null }
            if (toCheck != null) {
                posToCheck.add(toCheck)
            }
        }
    }
    posToCheck.remove(this.x to this.y)
    return Seat(this.filled, this.x, this.y, posToCheck)
}

fun printGrid(grid: Map<Pair<Int, Int>, Seat>) {
    for (x in 0..9) {
        for (y in 0..9) {
            print(when (grid[x to y]?.filled) {
                true -> '#'
                false -> 'L'
                else -> '.'
            })
        }
        println("")
    }
    print("\n")
}

fun doPass(rules: Int, grid: Map<Pair<Int, Int>, Seat>): Map<Pair<Int, Int>, Seat> {
    return grid.map { (key, value) -> key to applyRules(rules, grid, value) }.toMap()
}

fun applyRules(rules: Int, grid: Map<Pair<Int, Int>, Seat>, seat: Seat): Seat {
    val countFilled = seat.posToCheck.count { grid[it]?.filled ?: false }

    return Seat(when {
        !seat.filled && countFilled == 0 -> true
        seat.filled && countFilled >= rules -> false
        else -> seat.filled
    }, seat.x, seat.y, seat.posToCheck)
}


data class Seat(var filled: Boolean, val x: Int, val y: Int, val posToCheck: List<Pair<Int, Int>>) {

    constructor(filled: Boolean, x: Int, y: Int) : this(filled, x, y,getNeighbors(x, y))
}

fun getNeighbors(seatX: Int, seatY: Int): List<Pair<Int, Int>> {
    val posToCheck = mutableListOf<Pair<Int, Int>>()
    for (x in seatX - 1..seatX + 1) {
        for (y in seatY - 1..seatY + 1) {
            posToCheck.add(x to y)
        }
    }
    posToCheck.remove(seatX to seatY)
    return posToCheck
}