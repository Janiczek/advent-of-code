class Day10 {
    companion object {
        const val inputFile = "input202210.txt"

        fun parse1(input: String) = input.lines().map { parseInstruction(it) }
        fun parse2(input: String) = parse1(input)
        fun compute1(input: List<Instruction>): Int {
            return input.fold(initCpu to 0) { (cpu, strength), inst ->
                val newCpu = update1(inst, cpu)
                val cycleNum: Int? = findCycleNum(cpu, newCpu)
                val newStrength =
                    if (cycleNum != null)
                        strength + cycleNum * cpu.regX
                    else
                        strength
                newCpu to newStrength
            }.second
        }

        fun compute2(input: List<Instruction>): String {
            val width = 40
            val height = 6
            val result = input.fold(initCpu to setOf<Pair<Int,Int>>()) { (cpu, display), inst ->
                val newCpu = update1(inst, cpu)
                val newPixels: List<Pair<Int,Int>> =
                    (cpu.ticks until newCpu.ticks)
                        .filter { hit(it % width, cpu.regX) }
                        .map { it % width to it / width }
                val newDisplay = display + newPixels
                newCpu to newDisplay
            }.second
            return "\n" +
                    (0 until height).joinToString("\n") { y ->
                        (0 until width).joinToString("") { x ->
                            if (result.contains(x to y)) "█" else "░"
                        }
                    }
        }

        private fun hit(x: Int, maskCenter: Int): Boolean =
            x in maskCenter-1..maskCenter+1

        // COMMON

        sealed class Instruction
        object NoOp : Instruction()
        data class AddX(val i: Int): Instruction()

        private fun parseInstruction(line: String): Instruction {
            val words = line.split(" ")
            return when (words[0]) {
                "noop" -> NoOp
                "addx" -> AddX(words[1].toInt())
                else -> TODO("What")
            }
        }

        data class CPU(
            val regX: Int,
            val ticks: Int,
        )

        private val initCpu = CPU(1, 0)

        // PART 1

        private fun update1(inst: Instruction, cpu: CPU) =
            when (inst) {
                NoOp -> cpu.copy(ticks = cpu.ticks + 1)
                is AddX -> CPU(cpu.regX + inst.i, cpu.ticks + 2)
            }

        private val cycles = listOf(20,60,100,140,180,220)
        private fun findCycleNum(cpu: CPU, newCpu: CPU): Int? =
            cycles.firstOrNull { it > cpu.ticks && it <= newCpu.ticks }

        // PART 2


    }
}