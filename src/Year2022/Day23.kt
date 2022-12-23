import Day23.Companion.Entity.*
import Day23.Companion.Dir.*
import Day23.Companion.Dir

class Day23 {
    companion object {
        const val inputFile = "input202223.txt"

        enum class Entity {Space,Elf}
        enum class Dir {N,S,W,E}

        fun parse1(input: String): Map<XY,Entity> =
            input.lines().mapIndexed { y,line ->
                line.mapIndexed { x,char ->
                    val entity = when (char) {
                        '.' -> Space
                        '#' -> Elf
                        else -> TODO("parse1")
                    }
                    (x.toLong() to y.toLong()) to entity
                }
            }.flatten().toMap()
        fun parse2(input: String) = parse1(input)

        private fun dirListForRound(round: Long): List<Dir> =
            when (round % 4) {
                0L -> listOf(N,S,W,E)
                1L -> listOf(S,W,E,N)
                2L -> listOf(W,E,N,S)
                3L -> listOf(E,N,S,W)
                else -> TODO("Math broke")
            }

        fun compute1(input: Map<XY,Entity>): Long {
            var round = 0L
            val map = input.toMutableMap()
            //println("init")
            //println(map.draw(".", showBounds = false) { when (it) {
            //    Space -> "."
            //    Elf -> "#"
            //} })
            while (round < 10) {
                doRound(map,round)
                round++
            }
            val elfXYs = map.filterValues { it == Elf }.keys
            val minY = elfXYs.minOf { it.y }
            val maxY = elfXYs.maxOf { it.y }
            val minX = elfXYs.minOf { it.x }
            val maxX = elfXYs.maxOf { it.x }
            //println("smushed")
            //println("X: $minX..$maxX, Y: $minY..$maxY")
            //println(map.filterValues {it == Elf}.draw(".") { when (it) {
            //    Space -> "."
            //    Elf -> "#"
            //} })
            return (minX..maxX).times(minY..maxY)
                .count { (x,y) -> (map[x to y] ?: Space) != Elf }
                .toLong()
        }
        fun compute2(input: Map<XY,Entity>): Long {
            var round = 0L
            val map = input.toMutableMap()
            while (true) {
                val oldMap = map.toMap()
                doRound(map,round)
                round++
                if (map == oldMap) break
            }
            return round
        }

        private fun doRound(map: MutableMap<XY,Entity>, round: Long) {
            val list = dirListForRound(round)
            val elves: List<Pair<XY, Entity>> = map.filterValues { it == Elf }.toList()
            val wantedXYs: Map<XY, Int> = elves.map {
                if (it.first.neighbours().none { (map[it]?:Space) == Elf}) it.first
                else
                list.firstNotNullOfOrNull { dir ->
                    if (it.first.cone(dir).any { (map[it] ?: Space) == Elf }) null
                    else it.first.step(dir)
                } ?: it.first
            }.frequencies()
            val allowedXYs = wantedXYs.filterValues { it == 1 }.keys
            val wantedMoves: List<Pair<XY, XY>> = elves.mapNotNull {
                val wantedXY =
                    if (it.first.neighbours().none { (map[it]?:Space) == Elf}) null
                    else
                    list.firstNotNullOfOrNull { dir ->
                        if (it.first.cone(dir).any { (map[it]?:Space) == Elf }) null
                        else it.first.step(dir)
                    }
                if (wantedXY != null && wantedXY in allowedXYs) it.first to wantedXY
                else null
            }
            wantedMoves.forEach {
                map -= it.first
                map += it.second to Elf
            }
            //println("end of round ${round + 1}: $list")
            //println(map.draw(".", showBounds = false) {
            //    when (it) {
            //        Space -> "."
            //        Elf -> "#"
            //    }
            //})
        }
    }
}

fun XY.neighbours(): Set<XY> =
    setOf(
        this.decX().decY(), this.decY(), this.incX().decY(),
        this.decX(),                     this.incX(),
        this.decX().incY(), this.incY(), this.incX().incY(),
    )

private fun XY.step(dir: Dir): XY =
    when (dir) {
        N -> this.decY()
        S -> this.incY()
        W -> this.decX()
        E -> this.incX()
    }
private fun XY.cone(dir: Dir): Set<XY> =
    when (dir) {
        N -> setOf(this.decY(),this.decY().incX(),this.decY().decX())
        S -> setOf(this.incY(),this.incY().incX(),this.incY().decX())
        W -> setOf(this.decX(),this.decX().incY(),this.decX().decY())
        E -> setOf(this.incX(),this.incX().incY(),this.incX().decY())
    }
