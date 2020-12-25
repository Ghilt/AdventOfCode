fun main() {
    val input = "962713854".map { it.toString().toInt() }

    var state = input
    var currentCup = state[0]
    repeat(100) {
        val (next, newCurrent) = performMove(state, currentCup)
        state = next
        currentCup = newCurrent
    }
    println("Result p1: ${state.shift(1).dropLast(1).joinToString("")}")

    val stateP2 = (input + (1 + (input.maxOrNull() ?: 0)..1_000_000)).toMutableList()

    var nextIndex = 0
    repeat(10_000_000) {
        nextIndex = performMoveMutable(stateP2, nextIndex)
    }
    val iOf1 = stateP2.indexOf(1)
    println("Result p1: ${stateP2[(iOf1 + 1) % 1_000_000].toLong() * stateP2[(iOf1 + 2) % 1_000_000].toLong()}")
}

fun performMove(state: List<Int>, currentCup: Int): Pair<List<Int>, Int> {
    val nextIndex = 1 + state.indexOf(currentCup)
    val removed = List(3) { state.atCyclicIndex(nextIndex + it) }
    val newCurrent = state.atCyclicIndex(nextIndex + 3)
    var dc = currentCup
    do { if (dc == 1) dc = state.maxOf { it } else dc-- } while (dc in removed)

    return state.filter { it !in removed }.shift(dc).plus(removed) to newCurrent
}

// Part 2 opt with mutable list. Still took hours to run, but it's christmas so...
fun performMoveMutable(state: MutableList<Int>, currentIndex: Int): Int {
    val size = state.size
    val currentCup = state[currentIndex]
    var nextIndex = (1 + currentIndex) % size

    val fst = state.removeAt(nextIndex)
    nextIndex = if (nextIndex == 0) 0 else nextIndex % (size - 1)
    val snd = state.removeAt(nextIndex)
    nextIndex = if (nextIndex == 0) 0 else nextIndex % (size - 2)
    val thd = state.removeAt(nextIndex)
    nextIndex = if (nextIndex == 0) 0 else nextIndex % (size - 3)

    var dc = currentCup
    do { if (dc == 1) dc = 1_000_000 else dc-- } while (dc == fst || dc == snd || dc == thd)

    val di = state.indexOf(dc) + 1
    state.add(di, thd)
    state.add(di, snd)
    state.add(di, fst)
    return if (nextIndex < di) nextIndex else nextIndex + 3
}

private fun <E> List<E>.shift(dc: E): List<E> = takeLastWhile { it != dc } + takeWhile { it != dc } + dc

fun <T> List<T>.atCyclicIndex(i: Int) = this[i % this.size]