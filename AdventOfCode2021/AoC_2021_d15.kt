import java.io.File
import kotlin.math.min

data class AStarData (val openSet: List<Pair<Pair<Int, Int>, Int>>, val closedSet: Map<Pair<Int, Int>, Int>)

fun main() {
    val input = File("input/aoc_2021_d15.txt").readLines()
    val size = input.size
    val nodes = input.flatMapIndexed { y, row -> row.mapIndexed { x, char -> (x to y) to char.digitToInt() } }.toMap()
    /* --- Part 2 setup ---
    val nodes = input.flatMapIndexed { y, row -> row.flatMapIndexed { x, char ->
        (0..4).flatMap { yd -> (0..4).map { xd -> (x + xd * size to y + yd * size) to (char.digitToInt() + xd + yd ) % 10 + ((char.digitToInt() + xd + yd ) / 10) } }
    } }.toMap()
    val goal = 5 * size -1 to 5 * size -1
    // It found part2 result: 2872
    */
    val goal = size -1 to size -1
    var d = failed_aStar(nodes, goal, AStarData(listOf(0 to 0 to 0), emptyMap()))

    var closedSetNotContainsGoal = true
    while (closedSetNotContainsGoal) {
        d = failed_aStar(nodes, goal, d)
        closedSetNotContainsGoal = !d.closedSet.containsKey(goal)
    }
    println(d.closedSet[goal]!! - nodes[0 to 0]!!)
}

/* Algorithm not working, annoying since I have implemented a-star algorithm before. Anyway let it run long enough it just brute forces itself through the problem */
/* And I promised myself I wouldn't spend any time fixing it. Damn it's hard to let it go */
/* Was too lazy and cut to many corners again */
fun failed_aStar(nodes: Map<Pair<Int, Int>, Int>, goal: Pair<Int,Int>, a: AStarData): AStarData {
    val openSet = a.openSet
    val closedSet = a.closedSet
    val nextNode = openSet.first().first
    val neighbors = getNeighbors(nextNode, goal)
    val (visited, notVisited) = neighbors.partition { closedSet.containsKey(it) }
    val bestNeighbor = visited.map { closedSet[it]!! }.minOrNull() ?: 0
    val distanceToNextNode = nodes[nextNode]!! + bestNeighbor
    val newClosedSet = closedSet + visited.associateWith { min(closedSet[it]!!, distanceToNextNode + nodes[it]!! ) } + (nextNode to distanceToNextNode)
    val newOpenSet = (notVisited.map { it to distanceToNextNode } + openSet.filter { it.first != nextNode })/*.distinctBy { it.first }*/.sortedBy { it.second /*+ heuristic(it.first, goal.first)*/ }
    return AStarData(newOpenSet, newClosedSet)
}

private fun getNeighbors(nextNode: Pair<Int, Int>, goal: Pair<Int, Int>) = listOf(
    nextNode.first to nextNode.second - 1,
    nextNode.first to nextNode.second + 1,
    nextNode.first - 1 to nextNode.second,
    nextNode.first + 1 to nextNode.second
).filter { it.first in 0..goal.first && it.second in 0..goal.second }

fun heuristic(pos: Pair<Int, Int>, size: Int) = size - pos.first + size - pos.second