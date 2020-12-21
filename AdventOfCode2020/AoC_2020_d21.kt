import java.io.File

fun main() {
    val input = File("input/aoc_2020_d21.txt").readLines()
    val parsed = input.map { it.split("(").run { this[0].split(" ").dropLast(1) to this[1].dropLast(1).split(" ").drop(1).map { allergen -> allergen.filter { c -> c != ',' } } } }
    val ingredients = parsed.flatMap { it.first }.toSet()
    val allergens = parsed.flatMap { it.second }.toSet()
    val allergenSuspect = allergens.map { allergen -> allergen to parsed.filter { it.second.contains(allergen) }.map { it.first.toSet() }.reduce { acc, ingredients -> acc intersect ingredients } }
    val freeFromAllergens = ingredients - allergenSuspect.flatMap { it.second }.toSet()
    val countOfAllergenFree = parsed.flatMap { it.first }.count { freeFromAllergens.contains(it) }
    val allergenGuilty = mutableListOf<Pair<String, String>>()
    var allergenReduce = allergenSuspect
    while (allergenReduce.isNotEmpty()) {
        val (ingredient, _) = ingredients.map { it to allergenReduce.flatMap { allergens -> allergens.second }.count { ingredient -> ingredient == it } }.first { it.second == 1 }
        val (allergen, _) = allergenReduce.first { (_, ingredients) -> ingredients.contains(ingredient)}
        allergenGuilty.add(allergen to ingredient)
        allergenReduce = allergenReduce.map { (allergen, suspects) -> allergen to suspects.filter { it != ingredient }.toSet() }.filter { (a, _) -> allergen != a  }
    }
    val cdi = allergenGuilty.sortedBy { it.first }.joinToString(",") { it.second }
    println("Part1: $countOfAllergenFree")
    println("Part2: $cdi")
}