import Command.*
import java.io.File
import kotlin.math.abs


fun main() {
    val input = File("input/aoc_2020_d12.txt").readLines()

    val actions = input.map { it.toAction() }.fold(Ship()) { ship, instr -> ship.execute(instr) }
    println("Part 1: ${actions.x} ${actions.y} ---> ${actions.x + actions.y}")

    val wayPaintedShip = input.map { it.toAction() }.fold(WaypointShip()) { ship, instr -> ship.execute(instr) }
    println("Part 2: ${wayPaintedShip.ship.x} ${wayPaintedShip.ship.y} ---> ${abs(wayPaintedShip.ship.x) + abs(wayPaintedShip.ship.y)}")
}

private fun String.toAction(): Pair<Command, Int> = values().first { it.symbol == this[0] } to this.drop(1).toInt()

class WaypointShip(
        var ship: Ship = Ship(),
        var wayPointX: Int = 10,
        var wayPointY: Int = -1,
) {

    fun execute(action: Pair<Command, Int>): WaypointShip {
        when (action.first) {
            NORTH -> wayPointY -= action.second
            EAST -> wayPointX += action.second
            SOUTH -> wayPointY += action.second
            WEST -> wayPointX -= action.second
            LEFT -> {
                execute(RIGHT to 360 - action.second)
            }
            RIGHT -> {
                // Just because I got this as an interview question for my first job ^^
                // Swap two numbers without using a temp variable
                // It is obviously just confusing as hell
                repeat(action.second/90) {
                    wayPointX += wayPointY
                    wayPointY = wayPointX - wayPointY
                    wayPointX = -(wayPointX - wayPointY)
                }
            }
            FORWARD -> ship.goTowards(wayPointX, wayPointY, action.second)
        }
        return this
    }
}

class Ship(
        var x: Int = 0,
        var y: Int = 0,
        var direction: Command = EAST
) {

    fun execute(action: Pair<Command, Int>): Ship {
        when (action.first) {
            NORTH -> y -= action.second
            EAST -> x += action.second
            SOUTH -> y += action.second
            WEST -> x -= action.second
            LEFT -> direction = direction.turn(-action.second)
            RIGHT -> direction = direction.turn(action.second)
            FORWARD -> execute(direction to action.second)
        }
        return this
    }

    fun goTowards(wayPointX: Int, wayPointY: Int, distance: Int) {
        x += distance * wayPointX
        y += distance * wayPointY
    }
}

enum class Command(val symbol: Char) {
    NORTH('N'),
    EAST('E'),
    SOUTH('S'),
    WEST('W'),
    LEFT('L'),
    RIGHT('R'),
    FORWARD('F');

    fun turn(degrees: Int): Command = (this.toDegree() + degrees).toDirection()

    private fun toDegree(): Int = when (this) {
        NORTH -> -90
        EAST -> 0
        SOUTH -> 90
        WEST -> 180
        else -> throw RuntimeException("Not a degreeable command")
    }
}

private fun Int.toDirection(): Command = when (this % 360) {
    -90, 270 -> NORTH
    0 -> EAST
    90, -270 -> SOUTH
    180, -180 -> WEST
    else -> throw RuntimeException("Not a well defined angle")
}