@ExperimentalStdlibApi
fun main() {
    val sequence = mutableListOf(1, 0, 15, 2, 10, 13)

    val mapOfIndex = sequence.mapIndexed { index, v -> v to (index to -1) }.toMap().toMutableMap()

    val targetP1 = 2020
    val targetP2 = 30000000
    repeat(targetP2) {
        generateNextNumber(mapOfIndex, sequence)
    }

    println("Part 1: ${sequence[targetP1 - 1]} part 2: ${sequence[targetP2 - 1]}")
}

fun generateNextNumber(indexMap: MutableMap<Int, Pair<Int, Int>>, sequence: MutableList<Int>) {
    val previous = sequence.last()
    val prevPos = indexMap[previous] ?: -1 to -1
    val newIndex = sequence.size
    val oldIndexOf: (Int) -> Int = { (indexMap[it]?.first ?: -1) }
    if (prevPos.second == -1) {
        sequence.add(0)
        indexMap[0] = newIndex to oldIndexOf(0)
    } else {
        val new = sequence.size - (prevPos.second + 1)
        sequence.add(new)
        indexMap[new] = newIndex to oldIndexOf(new)
    }
}