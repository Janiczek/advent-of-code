import Day11.Companion.Op.Add
import Day11.Companion.Op.Mul

class Day11 {
    companion object {
        const val inputFile = "input202211.txt"

        fun parse1(input: String): List<Monkey> = input.split(Regex("\n\n")).map { parseMonkey(it) }
        fun parse2(input: String) = parse1(input)
        fun compute1(initMonkeys: List<Monkey>): Long {
            val lcm = initMonkeys.map { it.div }.fold(1) { acc,n -> acc * n }
            return go1(20,mapOf(),initMonkeys, 3, lcm)
        }
        fun compute2(initMonkeys: List<Monkey>): Long {
            val lcm = initMonkeys.map { it.div }.fold(1) { acc, n -> acc * n }
            return go1(10000, mapOf(), initMonkeys, 1, lcm)
        }

        // TYPES

        data class Monkey(
            val id: Int,
            val items: List<Int>,
            val fn: Pair<Op, What>,
            val div: Int,
            val ifTrue: Int,
            val ifFalse: Int,
        )

        enum class Op { Add, Mul }

        sealed class What
        object Old: What()
        data class Num(val n: Int): What()

        // PARSING

        private fun parseMonkey(input: String): Monkey {
            val lines = input.lines().map { it.trim().split(" ") }
            return Monkey(
                id = lines[0][1].filter { it.isDigit() }.toInt(),
                items = lines[1].map { it.filter { it.isDigit() } }.filterNot { it.isEmpty() }.map { it.toInt() },
                fn = parseFn(lines[2].drop(4)),
                div = lines[3][3].toInt(),
                ifTrue = lines[4][5].toInt(),
                ifFalse = lines[5][5].toInt(),
            )
        }

        private fun parseFn(expr: List<String>): Pair<Op, What> {
            val op = when (expr[0]) {
                "+" -> Add
                "*" -> Mul
                else -> TODO("parseFn: unknown operator")
            }
            val what = when (expr[1]) {
                "old" -> Old
                else -> Num(expr[1].toInt())
            }
            return op to what
        }

        // PART 1

        private tailrec fun go1(cyclesLeft: Int, inspectCounter: Map<Int,Long>, monkeys: List<Monkey>, divisor: Int, lcm: Int): Long {
            return if (cyclesLeft <= 0) {
                inspectCounter.toList().sortedByDescending { it.second }.take(2).fold(1) { acc, (_,n) -> acc * n }
            }
            else {
                val (newCounter, newMonkeys) = go1Cycle(monkeys.map { it.id }, inspectCounter, monkeys, divisor, lcm)
                go1(cyclesLeft - 1, newCounter, newMonkeys, divisor, lcm)
            }
        }

        private tailrec fun go1Cycle(todoMonkeyIds: List<Int>, inspectCounter: Map<Int,Long>, monkeys: List<Monkey>, divisor: Int, lcm: Int): Pair<Map<Int,Long>,List<Monkey>> {
            return if (todoMonkeyIds.isEmpty())
                inspectCounter to monkeys
            else {
                val monkeyId = todoMonkeyIds[0]
                val monkey = monkeys[monkeyId]
                val (newCounter, newMonkeys) = go1Item(monkey.items, monkey, inspectCounter, monkeys, divisor, lcm)
                go1Cycle(todoMonkeyIds.drop(1), newCounter, newMonkeys, divisor, lcm)
            }
        }

        private tailrec fun go1Item(todoItems: List<Int>, monkey: Monkey, inspectCounter: Map<Int,Long>, monkeys: List<Monkey>, divisor: Int, lcm: Int): Pair<Map<Int,Long>,List<Monkey>> {
            return if (todoItems.isEmpty())
                inspectCounter to monkeys
            else {
                val item = todoItems[0]
                val afterInspection = evalFn(item, monkey.fn, lcm) / divisor // already %'d!
                val throwTo =
                    if (afterInspection % monkey.div == 0) monkey.ifTrue
                    else monkey.ifFalse
                val newCounter = inspectCounter + (monkey.id to (inspectCounter[monkey.id]?.plus(1) ?: 1))
                val newMonkeys = monkeys.mapIndexed { i, m ->
                    when (i) {
                        throwTo -> m.copy(items = m.items + afterInspection)
                        monkey.id -> m.copy(items = m.items.drop(1))
                        else -> m
                    }
                }
                go1Item(todoItems.drop(1), monkey, newCounter, newMonkeys, divisor, lcm)
            }
        }

        private fun evalFn(item: Int, fn: Pair<Op, What>, lcm: Int): Int {
            val (op, what) = fn
            return when (op) {
                Add -> when (what) {
                    Old ->    (item + item)   % lcm
                    is Num -> (item + what.n) % lcm
                }
                Mul -> when (what) {
                    Old ->    item.toBigInteger().modPow(2.toBigInteger(), lcm.toBigInteger()).toInt()
                    is Num -> (item * what.n) % lcm
                }
            }
        }

    }
}