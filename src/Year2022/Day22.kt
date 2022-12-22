import Day22.Companion.Entity
import Day22.Companion.Entity.*
import Day22.Companion.Facing
import Day22.Companion.Facing.*

class Day22 {
    companion object {
        const val inputFile = "input202222.txt"

        enum class Facing {R,D,L,U}
        private fun Facing.turnLeft():  Facing = when (this) { R -> U; U -> L; L -> D; D -> R }
        private fun Facing.turnRight(): Facing = when (this) { R -> D; D -> L; L -> U; U -> R }
        enum class Entity {Void,Wall,Space}

        sealed class Step
        object TurnLeft: Step()
        object TurnRight: Step()
        data class Walk(val steps: Int): Step()


        fun parse1(input: String): Pair<Map<XY,Entity>,List<Step>> {
            val lines = input.lines()

            val mapLines = lines.dropLast(2)
            val map = parseMap(mapLines)

            val lastLine = lines.last()
            val steps = parseSteps(lastLine)

            return map to steps
        }

        fun parse2(input: String) = parse1(input)

        private fun parseMap(lines: List<String>): Map<XY, Entity> =
            lines.mapIndexed { y,line ->
                line.mapIndexed { x,char ->
                    val entity: Entity = when (char) {
                        '.' -> Space
                        '#' -> Wall
                        ' ' -> Void
                        else -> TODO("parse map")
                    }
                    (x.toLong() to y.toLong()) to entity
                }
            }.flatten().toMap()

        private fun parseSteps(line: String): List<Step> {
            val steps = mutableListOf<Step>()
            var todo = line
            while (todo.isNotEmpty()) {
                val (newStep, newTodo) = when (todo.first()) {
                    'L' -> TurnLeft to todo.drop(1)
                    'R' -> TurnRight to todo.drop(1)
                    else -> {
                        val toDrop = todo.takeWhile { it.isDigit() }
                        Walk(toDrop.toInt()) to todo.drop(toDrop.length)
                    }
                }
                steps += newStep
                todo = newTodo
            }
            return steps.toList()
        }

        fun compute1(input: Pair<Map<XY,Entity>,List<Step>>): Long {
            val (map,steps) = input
            val initXY = findStart(map)
            val initFacing = R
            val (finalXY, finalFacing) = step1(initXY,initFacing,steps,map)
            return final(finalXY, finalFacing)
        }

        private tailrec fun step1(xy: XY, facing: Facing, steps: List<Step>, map: Map<XY,Entity>): Pair<XY,Facing> =
            if (steps.isEmpty()) xy to facing
            else when (val step = steps.first()) {
                TurnLeft  -> step1(xy, facing.turnLeft(),  steps.drop(1), map)
                TurnRight -> step1(xy, facing.turnRight(), steps.drop(1), map)
                is Walk   -> step1(xy.advance1(facing, step.steps, map), facing, steps.drop(1), map)
            }
        fun compute2(input: Pair<Map<XY,Entity>,List<Step>>): Long {
            val (map,steps) = input
            val initXY = findStart(map)
            val initFacing = R
            val (finalXY, finalFacing) = step2(initXY,initFacing,steps,map)
            return final(finalXY, finalFacing)
        }
        private tailrec fun step2(xy: XY, facing: Facing, steps: List<Step>, map: Map<XY,Entity>): Pair<XY,Facing> =
            if (steps.isEmpty()) xy to facing
            else when (val step = steps.first()) {
                TurnLeft  -> step2(xy, facing.turnLeft(),  steps.drop(1), map)
                TurnRight -> step2(xy, facing.turnRight(), steps.drop(1), map)
                is Walk   -> {
                    val (newXY, newFacing) = xy.advance2(facing, step.steps, map)
                    step2(newXY, newFacing, steps.drop(1), map)
                }
            }
        private fun final(xy: XY, facing: Facing): Long =
            1000 * (xy.y + 1) + 4 * (xy.x + 1) + facing.ordinal

        private fun findStart(map: Map<XY, Entity>): XY =
            map
                .filterKeys { it.y == 0L }
                .firstNotNullOf { if (it.value == Space) it.key else null }

    }
}

private fun XY.advance1(facing: Facing, steps: Int, map: Map<XY, Entity>): XY {
    var xy = this
    for (i in 1..steps) {
        val (newXY, bounced) = xy.advance1Single(facing, map)
        xy = newXY
        if (bounced) break
    }
    return xy
}
private fun XY.advance1Single(facing: Facing, map: Map<XY, Entity>): Pair<XY,Boolean> {
    val triedXY = when (facing) {
        R -> this.addX(1)
        L -> this.subX(1)
        U -> this.subY(1)
        D -> this.addY(1)
    }
    return when (map[triedXY]) {
        Wall -> this to true
        Space -> triedXY to false
        else -> { // void, wraparound
            when (facing) {
                R -> {
                    val thisRow = map.filterKeys { it.y == this.y }.entries.sortedWith { a,b -> compareValuesBy(a,b, {it.key.y}, {it.key.x}) }
                    val leftmost = thisRow.first { it.value == Space || it.value == Wall }
                    if (leftmost.value == Space) leftmost.key to false
                    else this to true
                }
                L -> {
                    val thisRow = map.filterKeys { it.y == this.y }.entries.sortedWith { a,b -> compareValuesBy(b,a, {it.key.y}, {it.key.x}) }
                    val rightmost = thisRow.first { it.value == Space || it.value == Wall }
                    if (rightmost.value == Space) rightmost.key to false
                    else this to true
                }
                U -> {
                    val thisRow = map.filterKeys { it.x == this.x }.entries.sortedWith { a,b -> compareValuesBy(b,a, {it.key.y}, {it.key.x}) }
                    val downmost = thisRow.first { it.value == Space || it.value == Wall }
                    if (downmost.value == Space) downmost.key to false
                    else this to true
                }
                D -> {
                    val thisRow = map.filterKeys { it.x == this.x }.entries.sortedWith { a,b -> compareValuesBy(a,b, {it.key.y}, {it.key.x}) }
                    val topmost = thisRow.first { it.value == Space || it.value == Wall }
                    if (topmost.value == Space) topmost.key to false
                    else this to true
                }
            }
        }
    }
}
private fun XY.advance2(facing: Facing, steps: Int, map: Map<XY, Entity>): Pair<XY,Facing> {
    var xy = this
    var f = facing
    for (i in 1..steps) {
        val (newXY, newFacing, bounced) = xy.advance2Single(f, map)
        xy = newXY
        f = newFacing
        if (bounced) break
    }
    return xy to f
}

private val exampleCubeWraparound: Map<Pair<Int,Facing>,Pair<Int,Facing>> = mapOf(
        (1 to U) to (2 to D),
        (1 to L) to (3 to D),
        (1 to D) to (4 to D),
        (1 to R) to (6 to L),

        (2 to U) to (1 to D),
        (2 to L) to (6 to U),
        (2 to D) to (5 to U),
        (2 to R) to (3 to R),

        (3 to U) to (1 to R),
        (3 to L) to (2 to L),
        (3 to D) to (5 to R),
        (3 to R) to (4 to R),

        (4 to U) to (1 to U),
        (4 to L) to (3 to L),
        (4 to D) to (5 to D),
        (4 to R) to (6 to D),

        (5 to U) to (4 to U),
        (5 to L) to (3 to U),
        (5 to D) to (2 to U),
        (5 to R) to (6 to R),

        (6 to U) to (4 to L),
        (6 to L) to (5 to L),
        (6 to D) to (2 to R),
        (6 to R) to (1 to L),
    )
private val realCubeWraparound: Map<Pair<Int,Facing>,Pair<Int,Facing>> = mapOf(
        (1 to U) to (6 to R),
        (1 to L) to (4 to R),
        (1 to D) to (3 to D),
        (1 to R) to (2 to R),

        (2 to U) to (6 to U),
        (2 to L) to (1 to L),
        (2 to D) to (3 to L),
        (2 to R) to (5 to L),

        (3 to U) to (1 to U),
        (3 to L) to (4 to D),
        (3 to D) to (5 to D),
        (3 to R) to (2 to U),

        (4 to U) to (3 to R),
        (4 to L) to (1 to R),
        (4 to D) to (6 to D),
        (4 to R) to (5 to R),

        (5 to U) to (3 to U),
        (5 to L) to (4 to L),
        (5 to D) to (6 to L),
        (5 to R) to (2 to L),

        (6 to U) to (4 to U),
        (6 to L) to (1 to D),
        (6 to D) to (2 to D),
        (6 to R) to (5 to U),
    )
private fun exampleCubeFace(xy: XY): Int =
    if (xy.y < 4) 1
    else if (xy.y < 8 && xy.x < 4) 2
    else if (xy.y < 8 && xy.x < 8) 3
    else if (xy.y < 8) 4
    else if (xy.x < 12) 5
    else 6
private fun realCubeFace(xy: XY): Int =
    if (xy.y < 50 && xy.x < 100) 1
    else if (xy.y < 50) 2
    else if (xy.y < 100) 3
    else if (xy.y < 150 && xy.x < 50) 4
    else if (xy.y < 150) 5
    else 6
private fun exampleCubeOrigins(face: Int): XY =
    when (face) {
        1 -> 8L to 0L
        2 -> 0L to 4L
        3 -> 4L to 4L
        4 -> 8L to 4L
        5 -> 8L to 8L
        6 -> 12L to 8L
        else -> TODO("Whatttt")
    }
private fun realCubeOrigins(face: Int): XY =
    when (face) {
        1 -> 50L to 0L
        2 -> 100L to 0L
        3 -> 50L to 50L
        4 -> 0L to 100L
        5 -> 50L to 100L
        6 -> 0L to 150L
        else -> TODO("Whatttt")
    }
private const val exampleFaceSize = 4
private const val realFaceSize = 50

private const val faceSize = realFaceSize
private val cubeWraparound = realCubeWraparound
private fun cubeFace(xy: XY) = realCubeFace(xy)
private fun cubeOrigins(face: Int) = realCubeOrigins(face)

private fun XY.advance2Single(facing: Facing, map: Map<XY, Entity>): Triple<XY,Facing,Boolean> {
    val triedXY = when (facing) {
        R -> this.addX(1)
        L -> this.subX(1)
        U -> this.subY(1)
        D -> this.addY(1)
    }
    return when (map[triedXY]) {
        Wall -> { println("bumped to wall on $triedXY, staying on $this"); Triple(this, facing, true) }
        Space -> Triple(triedXY, facing, false)
        else -> { // void, wraparound
            val face = cubeFace(this)
            val (newFace, newFacing) = cubeWraparound[face to facing]!!
            println("wrapping: faces $face -> $newFace, facing $facing -> $newFacing")
            val wrappedXY = (this - cubeOrigins(face)).wrap(facing, newFacing) + cubeOrigins(newFace)
            println("wrapping: XY $this to (may yet bump) $wrappedXY")
            when (newFacing) {
                R -> {
                    val wrappedXYRow = map.filterKeys { it.y == wrappedXY.y }.entries.sortedWith { a,b -> compareValuesBy(a,b, {it.key.y}, {it.key.x}) }
                    val first = wrappedXYRow.first { it.value == Space || it.value == Wall }
                    if (first.value == Space) { println("wrapped, advanced to ${first.key}"); Triple(first.key,newFacing, false) }
                    else { println("bumped, didn't wrap, staying on $this"); Triple(this, facing, true) }
                }
                L -> {
                    val wrappedXYRow = map.filterKeys { it.y == wrappedXY.y }.entries.sortedWith { a,b -> compareValuesBy(b,a, {it.key.y}, {it.key.x}) }
                    val first = wrappedXYRow.first { it.value == Space || it.value == Wall }
                    if (first.value == Space) { println("wrapped, advanced to ${first.key}"); Triple(first.key,newFacing, false) }
                    else { println("bumped, didn't wrap, staying on $this"); Triple(this, facing, true) }
                }
                U -> {
                    val wrappedXYRow = map.filterKeys { it.x == wrappedXY.x }.entries.sortedWith { a,b -> compareValuesBy(b,a, {it.key.y}, {it.key.x}) }
                    val first = wrappedXYRow.first { it.value == Space || it.value == Wall }
                    if (first.value == Space) { println("wrapped, advanced to ${first.key}"); Triple(first.key,newFacing, false) }
                    else { println("bumped, didn't wrap, staying on $this"); Triple(this, facing, true) }
                }
                D -> {
                    val wrappedXYRow = map.filterKeys { it.x == wrappedXY.x }.entries.sortedWith { a,b -> compareValuesBy(a,b, {it.key.y}, {it.key.x}) }
                    val first = wrappedXYRow.first { it.value == Space || it.value == Wall }
                    if (first.value == Space) { println("wrapped, advanced to ${first.key}"); Triple(first.key,newFacing, false) }
                    else { println("bumped, didn't wrap, staying on $this"); Triple(this, facing, true) }
                }
            }
        }
    }
}

private fun XY.wrap(src: Facing, dst: Facing): XY =
    when (src to dst) {
        (U to U) -> this
        (U to L) -> TODO()
        (U to D) -> TODO()
        (U to R) -> this.transpose()

        (L to U) -> TODO()
        (L to L) -> this
        (L to D) -> this.transpose()
        (L to R) -> this.flipY()

        (D to U) -> this.flipX()
        (D to L) -> this.transpose()
        (D to D) -> this
        (D to R) -> TODO()

        (R to U) -> this.transpose()
        (R to L) -> this.flipY()
        (R to D) -> this.transpose().flipX().flipY()
        (R to R) -> this

        else -> TODO("what is missing?")
    }

operator fun XY.plus(b: XY): XY =
    (this.x + b.x) to (this.y + b.y)

operator fun XY.minus(b: XY): XY =
    (this.x - b.x) to (this.y - b.y)

fun XY.transpose(): XY = this.y to this.x
private fun XY.flipX(): XY = (faceSize - 1 - this.x) to this.y
private fun XY.flipY(): XY = this.x to (faceSize - 1 - this.y)
