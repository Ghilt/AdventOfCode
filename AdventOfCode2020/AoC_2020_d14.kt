import java.io.File
import java.math.BigInteger

typealias Memory = MutableMap<BigInteger, BigInteger>

@ExperimentalStdlibApi
fun main() {
    val input = File("input/aoc_2020_d14.txt").readLines()

    val instructions = input.fold(listOf<MaskedBlock>()) { acc, v ->
        when {
            v.startsWith("mask") -> acc.plus(MaskedBlock(v.split(" ").last()))
            else -> acc.dropLast(1).plus(
                    MaskedBlock(acc.last().mask, acc.last().instructions.plus(Instruction(v)))
            )
        }
    }

    val memory = executeBlocks(instructions, ::execute)
    println(memory.values.reduce { a, b -> a.plus(b) })

    val memory2 = executeBlocks(instructions, ::executePt2)
    println(memory2.values.reduce { a, b -> a.plus(b) })

}

fun executeBlocks(instructions: List<MaskedBlock>, executor: (Memory, String, Instruction) -> Unit) : Memory {
    val memory = mutableMapOf<BigInteger, BigInteger>()
    instructions.forEach { masked ->
        masked.instructions.forEach {
            executor(memory, masked.mask, it)
        }
    }
    return memory
}

fun execute(memory: Memory, mask: String, instr: Instruction) {
    memory[instr.address] = applyMask(mask, instr.value.toString(2).padStart(36, '0'))
}

fun applyMask(mask: String, valueToWrite: String): BigInteger {
    return BigInteger(mask.zip(valueToWrite) { c1, c2 ->
        when (c1) {
            'X' -> c2
            else -> c1
        }
    }.joinToString(""), 2)
}

fun executePt2(memory: Memory, mask: String, instr: Instruction) {
    unfloatBits(mask, instr.address.toString(2).padStart(36, '0')).forEach {
        memory[it] = instr.value
    }
}

fun unfloatBits(mask: String, valueToWrite: String): List<BigInteger> {
    // Transform this new mask format to the mask format of day 1 to reuse code
    val transformedToP1MaskFormat = mask.map {
        when (it) {
            'X' -> '0'
            '0' -> 'X'
            else -> '1'
        }
    }.joinToString("")
    val expanded = expandMasks(listOf(transformedToP1MaskFormat.toList()), 0)
    return expanded.map { applyMask(it.joinToString(""), valueToWrite) }
}

fun expandMasks(bits: List<List<Char>>, index: Int) : List<List<Char>> {
    if (index >= bits[0].size) {
        return bits
    }
    return if (bits[0][index] == '0') {
        expandMasks(bits.flatMap {
            listOf(it, it.mapIndexed{ i, c -> if (i == index) '1' else c })
        }, index + 1)
    } else {
        expandMasks(bits, index + 1)
    }
}

data class MaskedBlock(val mask: String, val instructions: List<Instruction> = listOf())
data class Instruction(val address: BigInteger, val value: BigInteger) {
    constructor(s: String) : this(BigInteger(s.split("]").first().drop(4)), BigInteger(s.split(" ").last()))
}