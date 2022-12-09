import Day09.Companion.Dir.*

class Day09 {
    companion object {
        const val inputFile = "input202209.txt"

        enum class Dir { R, U, L, D }
        data class Instruction (
            val dir: Dir,
            val amount: Int
        )

        fun parse1(input: String) = input.lines().map { parseInstruction(it) }
        fun parse2(input: String) = parse1(input)
        fun compute1(input: List<Instruction>) =
            input.fold(initState) { acc, instruction -> update1(acc, instruction) }.visitedTails.size

        fun compute2(input: List<Instruction>) =
            input.fold(initState2) { acc, instruction -> update2(acc, instruction) }.visitedTails.size

        private fun parseInstruction(line: String): Instruction {
            val amount = line.substring(2).toInt()
            return when (line[0]) {
                'R' -> Instruction(R, amount)
                'U' -> Instruction(U, amount)
                'L' -> Instruction(L, amount)
                'D' -> Instruction(D, amount)
                else -> TODO("What?")
            }
        }

        private fun dirToDelta(dir: Dir): Pair<Int,Int> =
            when (dir) {
                U -> 0 to 1
                D -> 0 to -1
                L -> -1 to 0
                R -> 1 to 0
            }

        private fun instructionToDelta(i: Instruction): Pair<Int,Int> =
            mulPos(dirToDelta(i.dir), i.amount)

        data class State (
            val head: Pair<Int,Int>,
            val tail: Pair<Int,Int>,
            val visitedTails: Set<Pair<Int,Int>>,
        )

        data class State2 (
            val head: Pair<Int,Int>,
            val t1:   Pair<Int,Int>,
            val t2:   Pair<Int,Int>,
            val t3:   Pair<Int,Int>,
            val t4:   Pair<Int,Int>,
            val t5:   Pair<Int,Int>,
            val t6:   Pair<Int,Int>,
            val t7:   Pair<Int,Int>,
            val t8:   Pair<Int,Int>,
            val tail: Pair<Int,Int>,
            val visitedTails: Set<Pair<Int,Int>>,
        )

        private val initPos = 0 to 0
        private val initState = State(
            head = initPos,
            tail = initPos,
            visitedTails = setOf(initPos),
        )
        private val initState2 = State2(
            head = initPos,
            t1   = initPos,
            t2   = initPos,
            t3   = initPos,
            t4   = initPos,
            t5   = initPos,
            t6   = initPos,
            t7   = initPos,
            t8   = initPos,
            tail = initPos,
            visitedTails = setOf(initPos),
        )

        private fun stepTail(pred: Pair<Int,Int>, succ: Pair<Int,Int>): Pair<Int,Int> {
            val deltaTail =
                if (touches(pred, succ))           (0 to 0)
                else if (sameRowOrCol(pred, succ)) deltaOrthogonal(pred, succ)
                else                               deltaDiagonal(pred, succ)
            return addPos(succ, deltaTail)
        }

        private fun update1(accState: State, instruction: Instruction): State =
            go1(accState, instruction.amount, instruction.dir)

        private fun go1(accState: State, todo: Int, dir: Dir): State {
            if (todo == 0) return accState
            else {
                val innerInstruction = Instruction(dir, 1)
                val deltaHead = instructionToDelta(innerInstruction)
                val newHead = addPos(accState.head, deltaHead)
                val newTail = stepTail(newHead, accState.tail)
                val newVisitedTails = accState.visitedTails + newTail
                return go1(
                    State(
                        head = newHead,
                        tail = newTail,
                        visitedTails = newVisitedTails,
                    ), todo - 1, dir
                )
            }
        }

        private fun update2(accState: State2, instruction: Instruction): State2 =
            go2(accState, instruction.amount, instruction.dir)

        private fun go2(accState: State2, todo: Int, dir: Dir): State2 {
            if (todo == 0) return accState
            else {
                val innerInstruction = Instruction(dir, 1)
                val newHead = addPos(accState.head, instructionToDelta(innerInstruction))
                val newT1   = stepTail(newHead, accState.t1)
                val newT2   = stepTail(newT1,   accState.t2)
                val newT3   = stepTail(newT2,   accState.t3)
                val newT4   = stepTail(newT3,   accState.t4)
                val newT5   = stepTail(newT4,   accState.t5)
                val newT6   = stepTail(newT5,   accState.t6)
                val newT7   = stepTail(newT6,   accState.t7)
                val newT8   = stepTail(newT7,   accState.t8)
                val newTail = stepTail(newT8,   accState.tail)
                val newVisitedTails = accState.visitedTails + newTail
                return go2(
                    State2(
                        head = newHead,
                        t1 = newT1,
                        t2 = newT2,
                        t3 = newT3,
                        t4 = newT4,
                        t5 = newT5,
                        t6 = newT6,
                        t7 = newT7,
                        t8 = newT8,
                        tail = newTail,
                        visitedTails = newVisitedTails,
                    ), todo - 1, dir
                )
            }
        }

        private fun touches(head: Pair<Int, Int>, tail: Pair<Int, Int>): Boolean =
            (head == tail)
                || allNeighbours(head).contains(tail)

        private fun sameRowOrCol(head: Pair<Int, Int>, tail: Pair<Int, Int>): Boolean =
            head.first == tail.first || head.second == tail.second

        private fun allNeighbours(head: Pair<Int, Int>): Set<Pair<Int,Int>> =
            listOf(
                -1 to -1, 0 to -1, 1 to -1,
                -1 to  0, 0 to  0, 1 to  0,
                -1 to  1, 0 to  1, 1 to  1,
            ).map { addPos(head, it) }.toSet()

        private fun addPos(a: Pair<Int,Int>, b: Pair<Int,Int>) =
            (a.first + b.first) to (a.second + b.second)

        private fun mulPos(a: Pair<Int,Int>, b: Int) =
            (a.first * b) to (a.second * b)

        private fun deltaOrthogonal(head: Pair<Int, Int>, tail: Pair<Int, Int>): Pair<Int, Int> =
            if (head.first == tail.first) (if (head.second > tail.second) 0 to 1 else  0 to -1)
            else                          (if (head.first > tail.first)   1 to 0 else -1 to 0)

        private fun deltaDiagonal(head: Pair<Int, Int>, tail: Pair<Int, Int>): Pair<Int, Int> =
            if (head.first < tail.first) (if (head.second < tail.second) -1 to -1 else -1 to 1)
            else                         (if (head.second < tail.second)  1 to -1 else  1 to 1)
    }
}