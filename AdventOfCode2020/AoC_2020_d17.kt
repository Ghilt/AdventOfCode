import java.io.File

typealias FindNeighbor = (Cell, Int, Int, Int, Int) -> Pos
typealias CountNeighbor = (Set<Cell>, Pos, Int, Int, Int, Int) -> Int

fun main() {
    val input = File("input/aoc_2020_d17.txt").readLines()

    val active = input.withIndex().flatMap { (y, row) ->
        row.mapIndexedNotNull { x, c ->
            when (c) {
                '#' -> Cell(x, y, 0, 0)
                else -> null
            }
        }
    }.toSet()
    println("Part1 ${active.size}")
    val find3DNeighbor: FindNeighbor = { c, x, y, z, _ -> Pos(c.x - x, c.y - y, c.z - z, 0) }
    val count3DNeighbor: CountNeighbor = { alive, p, x, y, z, w ->
        (!(x == 0 && 0 == y && 0 == z) && w == 0 && Cell(p.x - x, p.y - y, p.z - z, 0) in alive).toInt()
    }
    var cyclePart1 = active
    repeat(6) {
        cyclePart1 = simulate(cyclePart1, find3DNeighbor, count3DNeighbor)
        println("-> ${cyclePart1.size}")
    }
    println("Part2 ${active.size}")
    val findHyperNeighbor: FindNeighbor = { c, x, y, z, w -> Pos(c.x - x, c.y - y, c.z - z, c.w - w) }
    val countHyperNeighbor: CountNeighbor = { n, p, x, y, z, w ->
        (!(x == 0 && 0 == y && 0 == z && w == 0) && Cell(p.x - x, p.y - y, p.z - z, p.w - w) in n).toInt()
    }

    var cycleP2 = active
    repeat(6) {
        cycleP2 = simulate(cycleP2, findHyperNeighbor, countHyperNeighbor)
        println("-> ${cycleP2.size}")
    }
}

fun simulate(alive: Set<Cell>, findNeighbor: FindNeighbor, countNeighbor: CountNeighbor): Set<Cell> {
    return alive.mapWithNeighbors { c, x, y, z, w ->
        val pos = findNeighbor(c, x, y, z, w)
        val active = Cell(pos) in alive
        isAliveNextStep(pos, active, alive, countNeighbor)
    }.filter { it.aliveNext }.toSet()
}

fun isAliveNextStep(pos: Pos, imAlive: Boolean, alive: Set<Cell>, countNeighbor: CountNeighbor): Cell {
    val neighbors = listOf(pos).mapWithNeighbors { c, x, y, z, w -> countNeighbor(alive, c, x, y, z, w) }.sum()
    return Cell(pos, if (imAlive) neighbors == 3 || neighbors == 2 else neighbors == 3)
}

fun <R, T> Iterable<R>.mapWithNeighbors(action: (R, Int, Int, Int, Int) -> T): List<T> {
    // It was quite a bit faster when the summation of neighbors was done with fold, but It had to give way to one size fits all map
    return flatMap {
        (-1..1).flatMap { w ->
            (-1..1).flatMap { z ->
                (-1..1).flatMap { y ->
                    (-1..1).map { x ->
                        action(it, x, y, z, w)
                    }
                }
            }
        }
    }
}

data class Cell(val x: Int, val y: Int, val z: Int, val w: Int, val aliveNext: Boolean = true) {
    constructor(pos: Pos, aliveNext: Boolean = true) : this(pos.x, pos.y, pos.z, pos.w, aliveNext)
}

data class Pos(val x: Int, val y: Int, val z: Int, val w: Int)

fun Boolean.toInt() = if (this) 1 else 0