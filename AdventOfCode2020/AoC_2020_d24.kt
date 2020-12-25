import HexDir.valueOf
import java.io.File
import java.math.RoundingMode
import java.text.DecimalFormat
import kotlin.math.*

fun main() {
    val input = File("input/aoc_2020_d24.txt").readLines()
    val emptyChar: Char? = null
    val parsed = input.map {
        it.fold(listOf<HexDir>() to emptyChar) { (acc, mc), c ->
            when {
                mc != null -> acc.plus("$mc$c".toHexDir()) to null
                c == 'e' || c == 'w' -> acc.plus("$c".toHexDir()) to null
                else -> acc to c
            }
        }
    }.map { it.first }

    val flip = parsed.map {
        it.fold(Polar(0.0, 0.0)) { coordinate, instruction ->
            coordinate + instruction.vec
        }
    }.fold(setOf<Polar>()) { acc, flip ->
        if (flip in acc) acc.filter { flip != it }.toSet() else acc.plus(flip)
    }

    println("Result p1: ${flip.size}")

    var dayFlip = flip
    var adjacents: Set<Polar>
    repeat(100) {
        adjacents = setOf()
        dayFlip = dayFlip.mapNotNull { tile ->
            val neighbors = HexDir.values().map { it.vec + tile }
            val adjacentWhite = neighbors.filter { it !in dayFlip }
            adjacents = adjacents + adjacentWhite
            if (adjacentWhite.size == 6 || 6 - adjacentWhite.size > 2) null else tile
        }.toSet() + adjacents.mapNotNull { tile ->
            val neighbors = HexDir.values().map { it.vec + tile }
            val countNeighbor = neighbors.filter { it in dayFlip }.size
            if (countNeighbor == 2) tile else null
        }
    }
    println(dayFlip.size)

}

enum class HexDir(val vec: Polar) {
    E(Polar(1.0, 0.0)),
    SE(Polar(1.0, -PI / 3)),
    SW(Polar(1.0, -2 * PI / 3)),
    W(Polar(1.0, PI)),
    NE(Polar(1.0, PI / 3)),
    NW(Polar(1.0, 2 * PI / 3))
}

data class Polar(val radius: Double, val angle: Double) {
    operator fun plus(o: Polar): Polar {
        val diff = angle - o.angle
        val r = o.radius.pow(2) + radius.pow(2) + 2 * radius * o.radius * cos(diff)
        val t = o.angle + atan2(radius * sin(diff), o.radius + radius * cos(diff))
        return Polar(r.pow(0.5), t)
    }

    override fun equals(other: Any?): Boolean {
        return other is Polar
                && radius.dEquals(other.radius)
                && (angle.toPosAngle().rem(2 * PI).dEquals((other.angle.toPosAngle()).rem(2 * PI))
                || radius.dEquals(0.0))
    }

    override fun hashCode(): Int {
        val df = DecimalFormat("#.####")
        df.roundingMode = RoundingMode.HALF_UP

        val angleMatters = if (radius.dEquals(0.0)) 0 else df.format(angle.toPosAngle().rem(2 * PI)).hashCode()
        var result = df.format(radius).hashCode()
        result = 31 * result + angleMatters.hashCode()
        return result
    }
}

fun Double.toPosAngle() = 200 * PI + this

fun String.toHexDir() = valueOf(this.toUpperCase())

fun Double.dEquals(other: Double) = abs(this - other) < 0.001