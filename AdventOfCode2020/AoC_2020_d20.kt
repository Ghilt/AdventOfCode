import java.io.File
import kotlin.math.abs

typealias Image = MutableSet<Coord>

fun main() {
    val input = File("input/aoc_2020_d20.txt").readLines()
            .chunked(12).map { it.dropLast(1) }
            .map { Tile(it.first().substring(5..8).toInt(), it.drop(1), it.drop(1).getBorders()) }

    val matches = input.map {
        it.id to it.borders.flatMapIndexed { borderIndex, border ->
            input.filter { tile -> tile.id != it.id }
                    .filter { tile -> border in tile.borders }
                    .map { tile -> Connection(borderIndex, tile.id, tile.borders.indexOf(border)) }
        }
    }

    println("Result pt1: ${matches.map { it.first to it.second.size }.filter { it.second == 4 }.map { it.first.toLong() }.reduce { acc, id -> acc * id }}")

    val monster = setOf(18 to 0, 0 to 1, 5 to 1, 6 to 1, 11 to 1, 12 to 1, 17 to 1, 18 to 1, 19 to 1, 1 to 2, 4 to 2, 7 to 2, 10 to 2, 13 to 2, 16 to 2)

    val result = (0..7).map { imageOrientation ->
        val image = mutableSetOf<Coord>()

        val inserted = mutableMapOf<Tile, Int>()
        image.insertTile(inserted, input.first(), imageOrientation, Coord(0, 0))
        assemble(inserted, image, matches.associateBy { it.first }, input.associateBy { it.id }, input.first(), Coord(0, 0))

        val stitchedImage = image.removeBorders().stitch()

        val removeMonsters = stitchedImage.filterNot { (x, y) ->
            monster.any { (monsterSourceX, monsterSourceY) ->
                val shiftedMonster = monster.map { (usx, usy) -> Coord(usx - monsterSourceX, usy - monsterSourceY) }
                shiftedMonster.all { (shiftedX, shiftedY) ->
                    Coord(x + shiftedX, y + shiftedY) in stitchedImage
                }
            }
        }

        removeMonsters.size
    }.minOrNull()

    println("Result pt2: $result")
}

private fun Set<Coord>.stitch() = map { (x, y) -> Coord(x - 2 * (x / 10) - 1, y - 2 * (y / 10) - 1) }.toSet()

private fun Image.removeBorders(): Set<Coord> {
    val nx = abs(this.minByOrNull { it.x }?.x ?: 0)
    val ny = abs(this.minByOrNull { it.y }?.y ?: 0)
    val isBorderIndex = { i: Int -> i % 10 == 0 || i % 10 == 9 }
    return map { (x, y) -> Coord(x + nx, y + ny) }
            .filterNot { (x, y) -> isBorderIndex(x) || isBorderIndex(y) }.toSet()
}

private fun Image.insertTile(inserted: MutableMap<Tile, Int>, tile: Tile, orientation: Int, coord: Coord) {
    val img = tile.image.mapIndexed { y, row -> row.filterIndexed { x, _ -> y in 0..9 && x in 0..9 } }
    when (orientation) {
        0, 2, 4, 6 -> {
            img.reversed(orientation == 2 || orientation == 6).forEachIndexed { y, row ->
                row.reversed(orientation == 2 || orientation == 4).forEachIndexed { x, pixel ->
                    if (pixel == '#') this.add(Coord(10 * coord.x + x, 10 * coord.y + y))

                }
            }
        }
        1, 3, 5, 7 -> {
            img.reversed(orientation == 1 || orientation == 5).forEachIndexed { y, row ->
                row.reversed(orientation == 3 || orientation == 5).forEachIndexed { x, pixel ->
                    if (pixel == '#') this.add(Coord(10 * coord.x + y, 10 * coord.y + x))
                }
            }
        }
    }

    inserted[tile] = orientation
}

private fun String.reversed(b: Boolean) = this.toList().reversed(b)
private fun <E> List<E>.reversed(reverse: Boolean): List<E> = if (reverse) this.reversed() else this

fun assemble(inserted: MutableMap<Tile, Int>, image: Image, matches: Map<Int, Pair<Int, List<Connection>>>,
             input: Map<Int, Tile>, previous: Tile, tileCoord: Coord
) {
    val borderingPrevious = matches.getValue(previous.id).second
    val orientation = inserted.getValue(previous)
    val flipped = orientation > 3

    borderingPrevious.filter { flipped == it.borderIndex > 3 }.forEach { adjacent ->
        val tile = input.getValue(adjacent.neighborId)
        if (tile !in inserted.keys) {
            val (direction, orientationOfNew) = calculateOrientation(orientation, adjacent.borderIndex, adjacent.neighborBorderIndex)
            val insertAt = tileCoord.moveBy(direction)
            image.insertTile(inserted, tile, orientationOfNew, insertAt)
            assemble(inserted, image, matches, input, tile, insertAt)
        }
    }
}

fun calculateOrientation(imprintOrientation: Int, borderIndex: Int, neighborBorderIndex: Int): Pair<Int, Int> {
    // This breaks my brain
    val superRotated = superRotate(borderIndex, imprintOrientation)
    val whatWeWant = matchingBorder.getValue(superRotated)
    return superRotated to superRotate(neighborBorderIndex, whatWeWant)
}

fun superRotate(start: Int, by: Int): Int {
    val startPlusBy = start + by
    return when {
        by < 4 && start < 4 -> startPlusBy % 4
        by < 4 -> (startPlusBy - 4).rem(4)
        start < 4 -> ((startPlusBy - 4) % 4) + 4
        start == 4 || start == 6 -> startPlusBy % 4
        else -> (4 + by - start) % 4
    }
}

private fun List<String>.getBorders(): Set<String> {
    val left = this.flatMap { listOf(it.first()) }.joinToString("")
    val right = this.flatMap { listOf(it.last()) }.joinToString("")

    return setOf(
            /* Non flipped 0, 1, 2, 3*/
            this.first(), right, this.last().reversed(), left.reversed(),
            /* Flipped versions 4, 5, 6, 7*/
            this.first().reversed(), right.reversed(), this.last(), left
    )
}

private fun Coord.moveBy(direction: Int) = Coord(x + direction.toAlphaX(), y + direction.toAlphaY())

private fun Int.toAlphaY(): Int = when (this % 4) {
    0 -> -1
    2 -> 1
    else -> 0
}

private fun Int.toAlphaX() = when (this % 4) {
    1 -> 1
    3 -> -1
    else -> 0
}

val matchingBorder = listOf(0 to 6, 1 to 7, 2 to 4, 3 to 5).flatMap { (from, to) -> listOf(from to to, to to from) }.toMap()

data class Tile(val id: Int, val image: List<String>, val borders: Set<String>)
data class Coord(val x: Int, val y: Int)
data class Connection(val borderIndex: Int, val neighborId: Int, val neighborBorderIndex: Int)