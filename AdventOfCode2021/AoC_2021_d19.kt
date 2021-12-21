import java.io.File
import kotlin.math.abs

data class Pos(val x: Int, val y: Int, val z: Int) {
    override fun toString(): String = "($x,$y,$z)"

    operator fun plus(other: Pos) = Pos(x + other.x, y + other.y, z + other.z)
    operator fun minus(other: Pos) = Pos(x - other.x, y - other.y, z - other.z)
    fun distanceTo(other: Pos) = abs(x - other.x) + abs(y - other.y) + abs(z - other.z)
}

data class Connection(val rotate: Int, val from: Int, val to: Int, val translate: Pos)

fun main() {
    val input = File("input/aoc_2021_d19.txt").readText()
    val parse = input.split("--- scanner ..? ---".toRegex()).filter { it != "" }
        .map { text -> text.split("\n").filter { it.contains(",") }.map { fromText(it) } }

    val withRotations = parse.map { scanner -> scanner.map { allRotations(it) } }
    val transposedToScanners = withRotations.map { it.transpose(24) }
    val scannersTranslatedToAllTheirBeacons = transposedToScanners.map { rotatedScanners ->
        rotatedScanners.map { rotatedScanner ->
            rotatedScanner.indices.map { translateScannerOntoBeacon(rotatedScanner, it).toSet() }
        }
    }

    val connectionsList = mutableListOf<Connection>()

    /** The MEGATRON! Checks if a scanner overlaps with another by checking their rotated and translated versions against each other */
    for ((from, scannerMegatron) in scannersTranslatedToAllTheirBeacons.withIndex()
        .drop(1/* don't go through the first scanner as everything will be adjusted to its worldview*/)) {
        val foundPathTo = mutableSetOf<Int>()
        val toMegatrons = scannersTranslatedToAllTheirBeacons.withIndex().filter { it.value != scannerMegatron }
        for ((fromRotation, fromRotator) in scannerMegatron.withIndex()) {
            for ((fromTranslation, scannerTranslated) in fromRotator.withIndex()) {
                for ((to, toMegatron) in toMegatrons.filterNot { foundPathTo.contains(it.index) }) {
                    for ((toTranslation, scannerTranslated2) in toMegatron[0].withIndex()) {
                        val inCommon = beaconsInCommon(scannerTranslated, scannerTranslated2)
                        if (inCommon >= 12) {
                            //Since fromTranslation & toTranslation is set to origin in the translated map, but could've been any other of the 12 matches
                            val translation = transposedToScanners[to][0][toTranslation] - transposedToScanners[from][fromRotation][fromTranslation]
                            connectionsList.add(Connection(fromRotation, from, to, translation))
                            foundPathTo.add(to)
                        }
                    }
                }
            }
        }
    }

    // key - leadingTo - rotation - translation
    val scannerConversionMap = mutableMapOf(0 to (-1 to (0 to Pos(0, 0, 0))))
    val universe = parse[0].toMutableSet()

    val processConnections = connectionsList.toMutableList()
    val scanners = mutableListOf<Pos>()
    while (processConnections.isNotEmpty()) {
        val next = processConnections.first { scannerConversionMap.containsKey(it.to) }
        processConnections.remove(next)

        if (checkIfLoop(scannerConversionMap, next.from, next.to, 3)) continue // skip if this is a connection that causes a loop

        //----------- from scanner 1--- get to 0 ----- by doing-- this--- and that
        scannerConversionMap[next.from] = next.to to (next.rotate to next.translate)

        var cs = next.from
        var current = parse[cs]
        var scanner = Pos(0, 0, 0)
        while (cs != 0) {
            val (leadingTo, byRotByTran) = scannerConversionMap[cs]!!
            val (byRotation, byTranslation) = byRotByTran
            current = current.map { it.rotate(byRotation) + byTranslation }
            scanner = scanner.rotate(byRotation) + byTranslation
            cs = leadingTo
        }
        scanners.add(scanner)
        universe.addAll(current)
    }

    println("Result pt1: ${universe.size}")
    println("Result pt2: ${scanners.maxOf { from -> scanners.maxOf { to -> from.distanceTo(to) } }}")
}

fun checkIfLoop(map: Map<Int, Pair<Int, Pair<Int, Pos>>>, fromThis: Int, to: Int, loopDetectionMaxSize: Int): Boolean {
    val usedNode = mutableSetOf(fromThis)
    var lastLink = to
    for (step in 0 until loopDetectionMaxSize) {
        if (map.containsKey(lastLink)) {
            val newLink = map[lastLink]!!.first
            if (usedNode.contains(newLink)) {
                return true
            }
            usedNode.add(lastLink)
            lastLink = newLink
        } else {
            return false
        }
    }
    return false
}

fun beaconsInCommon(scannerTranslated: Set<Pos>, scannerTranslated2: Set<Pos>): Int {
    return scannerTranslated.count { scannerTranslated2.contains(it) }
}

fun translateScannerOntoBeacon(scanner: List<Pos>, indexOfTargetBeacon: Int): List<Pos> {
    val newOrigin = scanner[indexOfTargetBeacon]
    return scanner.map { newOrigin - it }
}

fun List<List<Pos>>.transpose(s: Int): List<List<Pos>> = fold((0 until s).map { mutableListOf<Pos>() }) { acc, rots ->
    acc.also { rots.forEachIndexed { i, pos -> acc[i].add(pos) } }
}

fun fromText(csv: String): Pos {
    val xyz = csv.filter { it.isDigit() || it == ',' || it == '-' }.split(",").map { it.toInt() }
    return Pos(xyz[0], xyz[1], xyz[2])
}

fun Pos.rotate(index: Int): Pos = allRotations(this)[index]

fun allRotations(point: Pos): List<Pos> {
    val (x, y, z) = point
    return listOf(
        Pos(x, y, z),
        Pos(x, -z, y),
        Pos(x, -y, -z),
        Pos(x, z, -y),
        Pos(-x, y, -z),
        Pos(-x, z, y),
        Pos(-x, -y, z),
        Pos(-x, -z, -y),

        Pos(-y, x, z),
        Pos(z, x, y),
        Pos(y, x, -z),
        Pos(-z, x, -y),
        Pos(y, -x, z),
        Pos(z, -x, -y),
        Pos(-y, -x, -z),
        Pos(-z, -x, y),

        Pos(-z, y, x),
        Pos(-y, -z, x),
        Pos(z, -y, x),
        Pos(y, z, x),
        Pos(y, -z, -x),
        Pos(-z, -y, -x),
        Pos(-y, z, -x),
        Pos(z, y, -x)
    )
}
