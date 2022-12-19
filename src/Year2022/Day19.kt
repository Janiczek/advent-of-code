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

        fun compute1(input: List<Blueprint>): Int {
            val minutes = 24
            return input.sumOf { useBlueprint(it, minutes) * it.id }
        }
        fun compute2(input: List<Blueprint>): Int = -1

        data class Todo(
            val minutesLeft: Int,
            val oreRobots: Int,
            val clayRobots: Int,
            val obsidianRobots: Int,
            val geodeRobots: Int,
            val ore: Int,
            val clay: Int,
            val obsidian: Int,
            val geodes: Int,
        )

        private fun useBlueprint(b: Blueprint, initMinutes: Int): Int {
            println("use blueprint $b")
            val start = Todo(
                minutesLeft = initMinutes,
                oreRobots = 1,
                clayRobots = 0,
                obsidianRobots = 0,
                geodeRobots = 0,
                ore = 0,
                clay = 0,
                obsidian = 0,
                geodes = 0,
            )
            val todos = mutableSetOf(start)
            var best = start
            while (todos.isNotEmpty()) {
                val todo = todos.first()
                todos -= todo
                //println("Doing todo $todo, there are ${todos.size} left")
                if (todo.geodes > best.geodes) best = todo
                if (todo.minutesLeft <= 0) continue
                val updated = todo.copy(
                    minutesLeft = todo.minutesLeft - 1,
                    ore = todo.ore + todo.oreRobots,
                    clay = todo.clay + todo.clayRobots,
                    obsidian = todo.obsidian + todo.obsidianRobots,
                    geodes = todo.geodes + todo.geodeRobots,
                )
                val candidates = listOfNotNull(
                    updated,
                    if (todo.ore >= b.oreRobotOreCost) updated.copy(
                        ore = updated.ore - b.oreRobotOreCost,
                        oreRobots = updated.oreRobots + 1,
                    ) else null,
                    if (todo.ore >= b.clayRobotOreCost) updated.copy(
                        ore = updated.ore - b.clayRobotOreCost,
                        clayRobots = updated.clayRobots + 1,
                    ) else null,
                    if (todo.ore >= b.obsidianRobotOreCost && todo.clay >= b.obsidianRobotClayCost) updated.copy(
                        ore = updated.ore - b.obsidianRobotOreCost,
                        clay = updated.clay - b.obsidianRobotClayCost,
                        obsidianRobots = updated.obsidianRobots + 1,
                    ) else null,
                    if (todo.ore >= b.geodeRobotOreCost && todo.obsidian >= b.geodeRobotObsidianCost) updated.copy(
                        ore = updated.ore - b.geodeRobotOreCost,
                        obsidian = updated.obsidian - b.geodeRobotObsidianCost,
                        geodeRobots = updated.geodeRobots + 1,
                    ) else null,
                )
                todos += candidates
            }
            println("best: $best")
            return best.geodes
        }
    }
}

fun String.anyInt(): Int =
    this.filter { c -> c == '-' || c.isDigit() }.toInt()