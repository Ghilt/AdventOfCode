import java.io.File

fun main() {
    val input = File("input/aoc_2020_d19.txt").readLines()
    val rules = input.takeWhile { it != "" }.map {
        when {
            it.contains('"') -> it.toChaRule()
            else -> it.toRefRule()
        }
    }.associateBy { it.id }

    val messages = input.dropWhile { it != "" }.drop(1)

    val conforms = messages.map {
        val (success, matchLength) = applyRule(rules, rules.getValue(0), 0, it)
        success && matchLength == it.length
    }

    println("Result pt1: ${conforms.count { it }}")

    val expand8Rule = generateRules { size -> List(size + 1) { 42 } }
    val expand11Rule = generateRules { size -> List((size + 1) * 2) { if (it <= size ) 42 else 31 } }

    val conformingMessages = mutableSetOf<String>()
    expand11Rule.forEach { r11 ->
        expand8Rule.forEach { r8 ->
            val n2Rules = rules + listOf(Ref(8, r8, listOf()), Ref(11, r11, listOf())).associateBy { it.id }
            messages.map { message ->
                val (success, matchLength) = applyRule(n2Rules, n2Rules.getValue(0), 0, message)
                message to (success && matchLength == message.length)
            }.forEach { (message, success) -> if (success) conformingMessages.add(message) }
        }
    }

    println("Result pt2: ${conformingMessages.size}")
}

fun generateRules(repeatFill: (Int) -> List<Int> ) = (0..10).map { repeatFill(it) }

fun applyRule(rules: Map<Int, Rule>, rule: Rule, position: Int, it: String): Pair<Boolean, Int> {
    return when (rule) {
        is Cha -> (it.getOrElse(position) { 'x' } == rule.char) to (position + 1)
        is Ref -> {
            val (success, distance) = checkPath(rules, rule.r1, position, it)
            return if (success) {
                success to distance
            } else {
                checkPath(rules, rule.r2, position, it)
            }
        }
    }
}

fun checkPath(rules: Map<Int, Rule>, refs: List<Int>, position: Int, message: String): Pair<Boolean, Int> {
    val (success, distance) = refs.fold(true to position) { (stillOk, dist), ruleId ->
        if (stillOk) {
            val (stillGood, traveled) = applyRule(rules, rules.getValue(ruleId), dist, message)
            (stillOk && stillGood) to (traveled)
        } else {
            false to 0
        }
    }
    return (refs.isNotEmpty() && success) to (distance)
}

fun String.toChaRule(): Cha {
    val split = this.split(":")
    return Cha(split[0].toInt(), split[1].split("\"")[1].first())
}

fun String.toRefRule(): Ref {
    val split = this.split(":")
    val refs = split[1].split("|").map { rs -> rs.split(" ").mapNotNull { it.toIntOrNull() } }
    return Ref(split[0].toInt(), refs[0], refs.getOrElse(1) { listOf() })
}

sealed class Rule(open val id: Int)
data class Cha(override val id: Int, val char: Char) : Rule(id)
data class Ref(override val id: Int, val r1: List<Int>, val r2: List<Int>) : Rule(id)