import java.io.File

fun main(args: Array<String>){  
    val input = File("input/aoc_2019_d2.txt").readLines()[0]

    // Part 1
    println( "Part 1: ${startSolve(input, 12, 2)}")

    // part 2
    for (noun in 1..99) {
        for (verb in 1..99) {
            val result = startSolve(input, noun, verb)
            if (result == 19690720) {
                print("Part 2: $noun$verb " )
                return
            }
        }
    }
}

private fun startSolve(inp: String, noun: Int, verb: Int): Int {
    val ops: List<MutableList<Int>> = inp
        .split(',')
        .mapNotNull { it.toIntOrNull() }
        .windowed(4, 4, true) // Well I really wanted to use that function, would've been so much simpler to just step by 4 normally
        .map { it.toMutableList() }

    ops[0][1] = noun
    ops[0][2] = verb

    for (op in ops) {
        if (op[0] == 99) {
            return ops[0][0]
        } else {
            ops.apply {
                val value = op[0].execAsOp(getByAbsPos(op[1]), getByAbsPos(op[2]))
                setByAbsPos(op[3], value)
            }
        }
    }
    return -1
}


private fun Int.execAsOp(a: Int, b: Int): Int {
    return when (this){
        1 -> a + b
        2 -> a * b
        else -> throw Exception("Unknown operation")
    }
}

private fun Int.toWindowedCoord(): Pair<Int, Int> = Pair(this / 4, this % 4)

private fun List<List<Int>>.getByAbsPos(absPos: Int): Int {
    val (window, position) = absPos.toWindowedCoord()
    return this[window][position]
}

private fun List<MutableList<Int>>.setByAbsPos(absPos: Int, value: Int) {
    val (window, position) = absPos.toWindowedCoord()
    this[window][position] = value
}