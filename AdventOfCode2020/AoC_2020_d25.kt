import java.io.File
import kotlin.math.*

val style = 20201227

fun main() {
    val input = File("input/aoc_2020_d25.txt").readLines()

    val cardPK = input[0].toLong()
    val doorPK = input[1].toLong()
    val doorLs = bruteForceLoopSize(7, doorPK)

    val encKey = transformSubject(cardPK, doorLs)

    println("Encryption key: $encKey")

}

fun bruteForceLoopSize(subject: Long, target: Long): Long {
    var ls = 0L
    var result = 0L
    while (result != target) {
        ls++
        result = transformSubject(subject, ls)
    }
    return ls
}

fun transformSubject(subject: Long, loopSize: Long): Long {
    var value = 1L
    for (i in 1L..loopSize) {
        value = (value * subject).rem(style)
    }
    return value
}
