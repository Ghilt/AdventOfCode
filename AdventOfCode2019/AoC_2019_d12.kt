import java.io.File
import kotlin.math.absoluteValue

fun main() {
    val input = File("input/aoc_2019_d12.txt").readLines()
    val parseRegex = """[^\-,\d]""".toRegex()

    val moons = input.map { row ->
        Moon(row.replace(parseRegex, "").split(",").map { it.toIntOrNull() })
    }

    val pairs = moons.foldIndexed(mutableListOf<Pair<Moon, Moon>>()) { i, acc, moon -> acc.addPairs(i, moons, moon) }

    for (i in 0 until 1000) {
        for ((m1, m2) in pairs) {
            m1.tickGravity(m2)
            m2.tickGravity(m1)
        }
        moons.forEach { it.tickVelocity() }
    }

    val p1 = moons.fold(0) { acc, m -> acc + m.energy }

    println("part 1: $p1")
}

fun <T> MutableList<Pair<T, T>>.addPairs(index: Int, all: List<T>, moon: T): MutableList<Pair<T, T>> {
    val range = 1 + index until all.size
    for (i in range) {
        this.add(Pair(moon, all[i]))
    }
    return this
}


data class Moon(val c: IntArray, val v: IntArray = IntArray(3)) {
    constructor(list: List<Int?>) : this(intArrayOf(list[0] ?: 0, list[1] ?: 0, list[2] ?: 0))

    val energy
        get() = c.fold(0) { acc, v -> acc + v.absoluteValue } * v.fold(0) { acc, v -> acc + v.absoluteValue }

    fun tickGravity(other: Moon) {
        for (i in c.indices) {
            if (c[i] > other.c[i]) {
                v[i] = v[i] - 1
            } else if (c[i] < other.c[i]) {
                v[i] = v[i] + 1
            }
        }
    }

    fun tickVelocity() {
        for (i in c.indices) {
            c[i] = c[i] + v[i]
        }
    }
}