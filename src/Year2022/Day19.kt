import java.util.*
import kotlin.math.ceil
import kotlin.math.max

class Day19 {
    companion object {
        const val inputFile = "input202219.txt"

        data class Blueprint(
            val id: Int,
            val oreRobotOreCost: Int,
            val clayRobotOreCost: Int,
            val obsidianRobotOreCost: Int,
            val obsidianRobotClayCost: Int,
            val geodeRobotOreCost: Int,
            val geodeRobotObsidianCost: Int,
        )

        fun parse1(input: String): List<Blueprint> =
            input.lines().map { parseLine(it) }
        fun parse2(input: String) = parse1(input)

        private fun parseLine(line: String): Blueprint {
            val words = line.split(" ")
            val id = words[1].anyInt()
            val oreRobotCost = words[6].toInt()
            val clayRobotCost = words[12].toInt()
            val obsidianRobotOreCost = words[18].toInt()
            val obsidianRobotClayCost = words[21].toInt()
            val geodeRobotOreCost = words[27].toInt()
            val geodeRobotObsidianCost = words[30].toInt()
            return Blueprint(
                id,
                oreRobotCost,
                clayRobotCost,
                obsidianRobotOreCost,
                obsidianRobotClayCost,
                geodeRobotOreCost,
                geodeRobotObsidianCost,
            )
        }

        fun compute1(input: List<Blueprint>): Int =
            input.sumOf { useBlueprint(it, 24) * it.id }

        fun compute2(input: List<Blueprint>): Int =
            input.take(3).map { useBlueprint(it, 32) }.fold(1) { a,b -> a * b }

        data class Todo(
            val minutesElapsed: Int,
            val oreRobots: Int,
            val clayRobots: Int,
            val obsidianRobots: Int,
            val geodeRobots: Int,
            val ore: Int,
            val clay: Int,
            val obsidian: Int,
            val geodes: Int,
            val path: List<Todo>,
        ) {
            override fun toString(): String =
                if (path.isEmpty()) listOf("[${minutesElapsed}]",oreRobots,clayRobots,obsidianRobots,geodeRobots,ore,clay,obsidian,geodes).joinToString(",")
                else listOf("[${minutesElapsed}]",oreRobots,clayRobots,obsidianRobots,geodeRobots,ore,clay,obsidian,geodes," <- ${path.reversed().joinToString(" <- ")}]").joinToString(",")
            fun advance(n: Int, maxMinutes: Int): Todo? {
                val m = n + 1
                return if (minutesElapsed + m <= maxMinutes)
                    copy(
                        minutesElapsed = minutesElapsed + m,
                        ore = ore + m * oreRobots,
                        clay = clay + m * clayRobots,
                        obsidian = obsidian + m * obsidianRobots,
                        geodes = geodes + m * geodeRobots,
                    ) else null
            }
            fun buildOreRobot(b: Blueprint): Todo? =
                if (ore >= b.oreRobotOreCost)
                    copy(
                        oreRobots = oreRobots + 1,
                        ore = ore - b.oreRobotOreCost,
                    ) else null
            fun buildClayRobot(b: Blueprint): Todo? =
                if (ore >= b.clayRobotOreCost)
                    copy(
                        clayRobots = clayRobots + 1,
                        ore = ore - b.clayRobotOreCost,
                    ) else null
            fun buildObsidianRobot(b: Blueprint): Todo? =
                if (ore >= b.obsidianRobotOreCost && clay >= b.obsidianRobotClayCost)
                    copy(
                        obsidianRobots = obsidianRobots + 1,
                        ore = ore - b.obsidianRobotOreCost,
                        clay = clay - b.obsidianRobotClayCost,
                    ) else null
            fun buildGeodeRobot(b: Blueprint): Todo? =
                if (ore >= b.geodeRobotOreCost && obsidian >= b.geodeRobotObsidianCost)
                    copy(
                        geodeRobots = geodeRobots + 1,
                        ore = ore - b.geodeRobotOreCost,
                        obsidian = obsidian - b.geodeRobotObsidianCost,
                    ) else null
            fun projectedGeodes(maxMinutes: Int): Int =
                geodes + (maxMinutes - minutesElapsed) * geodeRobots

            fun bestScenarioGeodes(maxMinutes: Int): Int =
                projectedGeodes(maxMinutes) +
                        triangular(geodeRobots + maxMinutes - minutesElapsed) -
                        triangular(geodeRobots - 1)
        }
        private fun minutesNeeded(cost: Int, current: Int, perMinute: Int): Int =
            ceil(max(0,(cost - current)).toFloat() / perMinute).toInt()

        private fun useBlueprint(B: Blueprint, maxMinutes: Int): Int {
            println("use blueprint $B")
            val start = Todo(
                minutesElapsed = 0,
                oreRobots = 1,
                clayRobots = 0,
                obsidianRobots = 0,
                geodeRobots = 0,
                ore = 0,
                clay = 0,
                obsidian = 0,
                geodes = 0,
                path = listOf(),
            )
            var best = start
            val todos = PriorityQueue<Todo> { a, b -> compareValuesBy(b, a) { it?.projectedGeodes(maxMinutes) } }
            todos.add(start)
            while (todos.isNotEmpty()) {
                val todo = todos.poll()
                if (todo.projectedGeodes(maxMinutes) > best.projectedGeodes(maxMinutes)) {
                    best = todo
                    println("found next best: $best, with projected geodes ${best.projectedGeodes(maxMinutes)}")
                    todos.removeIf { it.bestScenarioGeodes(maxMinutes) < best.projectedGeodes(maxMinutes)}
                    println("new #todos = ${todos.size}")
                }
                if (todo.minutesElapsed == maxMinutes) continue
                val proto = todo.copy(path = todo.path + todo.copy(path = listOf()))
                val candidates = listOfNotNull(
                    proto.advance(minutesNeeded(B.oreRobotOreCost, proto.ore, proto.oreRobots), maxMinutes)?.buildOreRobot(B),
                    proto.advance(minutesNeeded(B.clayRobotOreCost, proto.ore, proto.oreRobots), maxMinutes)?.buildClayRobot(B),
                    if (proto.clayRobots > 0)
                        proto.advance(maxOf(
                            minutesNeeded(B.obsidianRobotOreCost,  proto.ore,  proto.oreRobots),
                            minutesNeeded(B.obsidianRobotClayCost, proto.clay, proto.clayRobots),
                        ), maxMinutes)?.buildObsidianRobot(B) else null,
                    if (proto.obsidianRobots > 0)
                        proto.advance(maxOf(
                            minutesNeeded(B.geodeRobotOreCost,      proto.ore,      proto.oreRobots),
                            minutesNeeded(B.geodeRobotObsidianCost, proto.obsidian, proto.obsidianRobots),
                        ), maxMinutes)?.buildGeodeRobot(B) else null,
                )
                todos.addAll(candidates)
            }
            println("best: $best, with projected geodes ${best.projectedGeodes(maxMinutes)}")
            return best.projectedGeodes(maxMinutes)
        }
    }
}

fun String.anyInt(): Int =
    this.filter { c -> c == '-' || c.isDigit() }.toInt()

fun triangular(n: Int) =
    n * (n + 1) / 2