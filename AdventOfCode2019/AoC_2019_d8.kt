import java.io.File

val input = File("input/aoc_2019_d8.txt").readLines()[0]
val width = 25
val height = 6

fun main(args: Array<String>) {

    val inp = input.map { Character.getNumericValue(it) }
    val layers = inp.chunked(width * height)
    val layerInfo = layers.map { l -> l.fold(mutableListOf(0, 0, 0)) { acc, e -> acc.plusCountOne(e) } }

    val target = layerInfo.sortedBy { it[0] }[0]

    println("part 1: $target")

    val fin = layers.fold(IntArray(width * height) { 2 }) { acc, layer ->
        layer.forEachIndexed { i, px -> if (acc[i] == 2) acc[i] = px }
        acc
    }

    val img = fin.mapIndexed { i, c -> "${addNewLine(i)}$c" }.joinToString("") { it }.replace("0", " ")

    println(img)
}

fun addNewLine(i: Int) = if (i % width == 0) "\n" else ""
fun MutableList<Int>.plusCountOne(i: Int): MutableList<Int> = apply { this[i] = this[i] + 1 }
