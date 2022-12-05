class Day05 {
    companion object {
        const val inputFile = "input202205.txt"

        fun parse1(input: String): Pair<List<List<Char>>,List<Triple<Int,Int,Int>>> {
            val nl = input.split("\n\n")
            val towerStr = nl[0]
            val transp = towerStr.lines().map { it.toList() }.transpose()
            val stacks = transp
                .filter { it.any { it.isLetter() } }
                .map { it.filter { it.isLetter() } }
            val instructions = nl[1].lines().map { parseInstruction(it) }
            return Pair(stacks, instructions)
        }
        fun parse2(input: String) = parse1(input)
        fun compute1(input: Pair<List<List<Char>>, List<Triple<Int, Int, Int>>>): String {
            val (stacks, instructions) = input
            val endStacks = instructions.fold(stacks) { acc, instr -> runInstruction(instr,acc) }
            return endStacks.map { it[0] }.joinToString("")
        }
        fun compute2(input: Pair<List<List<Char>>, List<Triple<Int, Int, Int>>>): String {
            val (stacks, instructions) = input
            val endStacks = instructions.fold(stacks) { acc, instr -> runInstruction(instr,acc,false) }
            return endStacks.map { it[0] }.joinToString("")
        }

        private fun parseInstruction(str: String): Triple<Int, Int, Int> {
            val words = str.split(" ")
            val count = words[1].toInt()
            val src = words[3].toInt() - 1
            val dst = words[5].toInt() - 1
            return Triple(count,src,dst)
        }

        private fun runInstruction(instr: Triple<Int,Int,Int>, stacks: List<List<Char>>, reverse: Boolean = true): List<List<Char>> {
            val (count, src, dst) = instr
            val taken = stacks[src].take(count)
            val takenReversed = if (reverse) taken.reversed() else taken
            val newStacks = stacks.toMutableList()
            newStacks[src] = newStacks[src].drop(count)
            newStacks[dst] = takenReversed + newStacks[dst]
            return newStacks.toList()
        }
    }
}

fun <T> List<List<T>>.transpose(): List<List<T>> {
    // [ [1,2,3,4], [5,6,7,8] ]
    // ->
    // [ [1,5], [2,6], [3,7], [4,8] ]
    val outerLength = this.size
    val innerLength = this.maxOf { it.size }
    val xs = mutableListOf<MutableList<T>>()
    for (i in 0 until innerLength) {
        xs.add(mutableListOf())
    }
    for (o in 0 until outerLength) {
        for (i in 0 until innerLength) {
            xs[i].add(this[o][i])
        }
    }
    return xs.map { it.toList() }.toList()
}