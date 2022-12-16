import Day16.Companion.BothDone
import Day16.Companion.BothGoing
import Day16.Companion.LeftDoneRightGoing
import Day16.Companion.LeftGoingRightDone
import Day16.Companion.Progress
import java.util.*
import kotlin.math.min

class Day16 {
    companion object {
        const val inputFile = "input202216.txt"

        data class Input(val list: List<Valve>)
        data class Valve(
            val name: String,
            val pressure: Int,
            val paths: List<String>,
        )

        fun parse1(input: String): Input = Input(input.lines().map { parseLine(it) })
        fun parse2(input: String) = parse1(input)
        private fun parseLine(line: String): Valve {
            val words = line.split(" ")
            val name = words[1]
            val pressure = words[4].filter { it.isDigit() }.toInt()
            val paths = words.drop(9).map { it.filter { c -> c != ',' } }
            return Valve(name, pressure, paths)
        }

        fun compute1(input: Input): Int {
            val maxMinutes = 30
            val usefulValves = input.list.filter { it.pressure > 0 }.map { it.name }.toSet()
            val m = input.list.associateBy { it.name }
            val shortestPaths: Map<Pair<String,String>,Int> = // without opening the valves
                usefulValves.map { dijkstra(it, m).mapKeys { kv -> it to kv.key } }.unionAll() +
                        dijkstra("AA",m).mapKeys { kv -> "AA" to kv.key }
            val shortestPathsWithOpening: Map<Pair<String,String>,Int> =
                shortestPaths
                    .filter { kv -> (kv.key.first == "AA" || kv.key.first in usefulValves) && kv.key.second in usefulValves }
                    .mapValues { kv -> kv.value + 1 }
            val dfsQueue = PriorityQueue<DfsItem>()
            val first = DfsItem(listOf("AA"), 0, 0, 0)
            dfsQueue.add(first)
            val best = dfs(dfsQueue, first, shortestPathsWithOpening, m, maxMinutes)
            return projected(best, maxMinutes)
        }

        data class TodoItem(
            val pos: String,
            val cost: Int,
        ) : Comparable<TodoItem> {
            override fun compareTo(other: TodoItem): Int =
                compareValuesBy(this, other) { it.cost }
        }

        // Find the shortest path costs from `pair.start` to all other nodes
        private fun dijkstra(start: String, m: Map<String, Valve>): Map<String, Int> {
            val costs: MutableMap<String,Int> = m.keys.associateWith { Int.MAX_VALUE }.toMutableMap()
            val visited: MutableSet<String> = mutableSetOf()
            val queue = PriorityQueue<TodoItem>()
            queue.add(TodoItem(start, 0))
            costs[start] = 0
            while (queue.isNotEmpty()) {
                val cell = queue.poll()
                if (visited.contains(cell.pos)) continue
                visited.add(cell.pos)
                (m[cell.pos]!!.paths)
                    .filter { !visited.contains(it) }
                    .forEach {
                        val newCost = minOf(
                            costs[it]!!,
                            costs[cell.pos]!! + 1,
                        )
                        costs[it] = newCost
                        queue.add(TodoItem(it, newCost))
                    }
            }
            return costs.toMap()
        }

        data class DfsItem(
            val path: List<String>,
            val minutesUsed: Int,
            val accumulatedPressure: Int,
            val pressurePerMinute: Int,
        ) : Comparable<DfsItem> {
            override fun compareTo(other: DfsItem): Int =
                compareValuesBy(this, other, { -it.accumulatedPressure }, { it.minutesUsed })
        }

        private tailrec fun dfs(todos: PriorityQueue<DfsItem>, best: DfsItem, paths: Map<Pair<String, String>, Int>, m: Map<String, Valve>, maxMinutes: Int): DfsItem {
            if (todos.isEmpty()) return best
            val todo = todos.poll()
            // TODO pruning? perhaps if all possible p/m improvements with current minutes left won't be enough to catch up to best?
            //println(${todo.path}, ${todo.minutesUsed}/${maxMinutes}, ${todo.pressurePerMinute} p/m, acc ${todo.accumulatedPressure}, with projected ${projected(todo)}")
            val last = todo.path.last()
            val nextCandidates: List<String> =
                paths
                    .filter { it.key.first == last && it.key.second !in todo.path }
                    .map { it.key.second }
            val newBest = if (projected(todo, maxMinutes) > projected(best, maxMinutes)) {
                //println("= new best!")
                todo
            } else best
            val newTodos: List<DfsItem> =
                nextCandidates
                    .mapNotNull {
                        val addedMinutes = paths[last to it]!!
                        val newMinutes = todo.minutesUsed + addedMinutes
                        if (newMinutes > maxMinutes) null
                        else {
                            val newPressurePerMinute = todo.pressurePerMinute + m[it]!!.pressure
                            val newAccPressure = todo.accumulatedPressure + todo.pressurePerMinute * addedMinutes
                            DfsItem(todo.path + it, newMinutes, newAccPressure, newPressurePerMinute)
                        }
                    }
            todos.addAll(newTodos)
            return dfs(todos, newBest, paths, m, maxMinutes)
        }

        private fun projected(todo: DfsItem, maxMinutes: Int): Int =
            todo.accumulatedPressure + (maxMinutes - todo.minutesUsed) * todo.pressurePerMinute

        fun compute2(input: Input): Int {
            val maxMinutes = 26
            val usefulValves = input.list.filter { it.pressure > 0 }.map { it.name }.toSet()
            val m = input.list.associateBy { it.name }
            val shortestPaths: Map<Pair<String,String>,Int> = // without opening the valves
                usefulValves.map { dijkstra(it, m).mapKeys { kv -> it to kv.key } }.unionAll() +
                        dijkstra("AA",m).mapKeys { kv -> "AA" to kv.key }
            val shortestPathsWithOpening: Map<Pair<String,String>,Int> =
                shortestPaths
                    .filter { kv -> (kv.key.first == "AA" || kv.key.first in usefulValves) && kv.key.second in usefulValves }
                    .mapValues { kv -> kv.value + 1 }
            val dfsQueue = PriorityQueue<DfsItem2>()
            val first = DfsItem2(listOf("AA"), listOf("AA"), BothDone, 0, 0, 0)
            dfsQueue.add(first)
            val best = dfs(dfsQueue, first, shortestPathsWithOpening, m, maxMinutes)
            println(best)
            return projected(best, maxMinutes, m)
        }

        sealed class Progress
        object BothDone: Progress()
        data class LeftGoingRightDone(val leftNode: String, val leftEta: Int): Progress()
        data class LeftDoneRightGoing(val rightNode: String, val rightEta: Int): Progress()
        data class BothGoing(val leftNode: String, val leftEta: Int, val rightNode: String, val rightEta: Int): Progress()


        data class DfsItem2(
            val path1: List<String>,
            val path2: List<String>,
            val progress: Progress,
            val minutesUsed: Int,
            val accumulatedPressure: Int,
            val pressurePerMinute: Int,
        ) : Comparable<DfsItem2> {
            override fun compareTo(other: DfsItem2): Int =
                compareValuesBy(this, other, { -it.accumulatedPressure }, { it.minutesUsed })
        }

        private tailrec fun dfs(todos: PriorityQueue<DfsItem2>, best: DfsItem2, paths: Map<Pair<String, String>, Int>, m: Map<String, Valve>, maxMinutes: Int): DfsItem2 {
            if (todos.isEmpty()) return best
            val todo = todos.poll()
            // Prune: if all improvements left won't catch up to best, continue
            if (projected(todo.copy(pressurePerMinute = m.values.sumOf { it.pressure }), maxMinutes, m) < projected(best, maxMinutes, m)) {
                //println("pruning ${todo.path1} ${todo.path2}")
                print("-")
                return dfs(todos, best, paths, m, maxMinutes) // continue
            }
            val last1 = todo.path1.last()
            val last2 = todo.path2.last()
            val nextCandidates1: List<String> =
                paths
                    .filter { it.key.first == last1 && it.key.second !in todo.path1 && it.key.second !in todo.path2 && it.key.second !in todo.progress.nodes() }
                    .map { it.key.second }
            val nextCandidates2: List<String> =
                paths
                    .filter { it.key.first == last2 && it.key.second !in todo.path1 && it.key.second !in todo.path2 && it.key.second !in todo.progress.nodes() }
                    .map { it.key.second }
            val newBest = if (projected(todo, maxMinutes, m) > projected(best, maxMinutes, m)) {
                println("new best (${todos.size} todos): ${todo.path1}, ${todo.path2}, progress ${todo.progress}, ${todo.minutesUsed}/${maxMinutes}, ${todo.pressurePerMinute} p/m, acc ${todo.accumulatedPressure}, with projected ${projected(todo, maxMinutes, m)}")
                todo
            } else best
            val newTodos: List<DfsItem2> =
                when (todo.progress) {
                    is LeftGoingRightDone -> {
                        // pick a right, change into BothGoing
                        nextCandidates2
                            .mapNotNull {
                                val neededMinutes = paths[last2 to it]!!
                                if (todo.minutesUsed + neededMinutes > maxMinutes) null
                                else todo.copy(progress = BothGoing(todo.progress.leftNode, todo.progress.leftEta, it, neededMinutes))
                            }
                    }
                    is LeftDoneRightGoing -> {
                        // pick a left, change into BothGoing
                        nextCandidates1
                            .mapNotNull {
                                val neededMinutes = paths[last1 to it]!!
                                if (todo.minutesUsed + neededMinutes > maxMinutes) null
                                else todo.copy(progress = BothGoing(it, neededMinutes, todo.progress.rightNode, todo.progress.rightEta))
                            }
                    }
                    BothDone -> {
                        // pick both, change into BothGoing
                        nextCandidates1.times(nextCandidates2)
                            .filter { it.first != it.second }
                            .mapNotNull {
                                val neededMinutes1 = paths[last1 to it.first]!!
                                val neededMinutes2 = paths[last2 to it.second]!!
                                if (todo.minutesUsed + min(neededMinutes1, neededMinutes2) > maxMinutes) null // neither would make it
                                else todo.copy(progress = BothGoing(it.first, neededMinutes1, it.second, neededMinutes2))
                            }
                    }
                    is BothGoing -> {
                        // find which is faster, based on LT EQ GT change into the other three options
                        // by the null-producing checks in the above cases we're guaranteed the ETA we'll advance will fit into maxMinutes
                        if (todo.progress.leftEta < todo.progress.rightEta) {
                            // change into LeftDoneRightGoing
                            listOf(DfsItem2(
                                path1 = todo.path1 + todo.progress.leftNode,
                                path2 = todo.path2,
                                progress = LeftDoneRightGoing(
                                    rightNode = todo.progress.rightNode,
                                    rightEta = todo.progress.rightEta - todo.progress.leftEta,
                                ),
                                minutesUsed = todo.minutesUsed + todo.progress.leftEta,
                                accumulatedPressure = todo.accumulatedPressure + todo.pressurePerMinute * todo.progress.leftEta,
                                pressurePerMinute = todo.pressurePerMinute + m[todo.progress.leftNode]!!.pressure,
                            ))
                        } else if (todo.progress.leftEta > todo.progress.rightEta) {
                            // change into LeftGoingRightDone
                            listOf(DfsItem2(
                                path1 = todo.path1,
                                path2 = todo.path2 + todo.progress.rightNode,
                                progress = LeftGoingRightDone(
                                    leftNode = todo.progress.leftNode,
                                    leftEta = todo.progress.leftEta - todo.progress.rightEta,
                                ),
                                minutesUsed = todo.minutesUsed + todo.progress.rightEta,
                                accumulatedPressure = todo.accumulatedPressure + todo.pressurePerMinute * todo.progress.rightEta,
                                pressurePerMinute = todo.pressurePerMinute + m[todo.progress.rightNode]!!.pressure,
                            ))
                        } else {
                            // both ETAs elapsed at the same time!
                            // change into BothDone
                            listOf(DfsItem2(
                                path1 = todo.path1 + todo.progress.leftNode,
                                path2 = todo.path2 + todo.progress.rightNode,
                                progress = BothDone,
                                minutesUsed = todo.minutesUsed + todo.progress.leftEta,
                                accumulatedPressure = todo.accumulatedPressure + todo.pressurePerMinute * todo.progress.leftEta,
                                pressurePerMinute = todo.pressurePerMinute +
                                        m[todo.progress.leftNode]!!.pressure +
                                        m[todo.progress.rightNode]!!.pressure,
                            ))
                        }
                    }
                }
            todos.addAll(newTodos)
            print(".")
            return dfs(todos, newBest, paths, m, maxMinutes)
        }

        private fun projected(todo: DfsItem2, maxMinutes: Int, m: Map<String, Valve>): Int =
            when (todo.progress) {
                BothDone ->
                    todo.accumulatedPressure +
                            (maxMinutes - todo.minutesUsed) * todo.pressurePerMinute

                is LeftGoingRightDone ->
                    todo.accumulatedPressure +
                            todo.progress.leftEta * todo.pressurePerMinute +
                            (maxMinutes - todo.minutesUsed - todo.progress.leftEta) * (todo.pressurePerMinute + m[todo.progress.leftNode]!!.pressure)

                is LeftDoneRightGoing ->
                    todo.accumulatedPressure +
                            todo.progress.rightEta * todo.pressurePerMinute +
                            (maxMinutes - todo.minutesUsed - todo.progress.rightEta) * (todo.pressurePerMinute + m[todo.progress.rightNode]!!.pressure)

                is BothGoing ->
                    if (todo.progress.leftEta < todo.progress.rightEta)
                            todo.accumulatedPressure +
                                    todo.progress.leftEta * todo.pressurePerMinute +
                                    (todo.progress.rightEta - todo.progress.leftEta) * (todo.pressurePerMinute + m[todo.progress.leftNode]!!.pressure) +
                                    (maxMinutes - todo.minutesUsed - todo.progress.rightEta) * (todo.pressurePerMinute + m[todo.progress.leftNode]!!.pressure + m[todo.progress.rightNode]!!.pressure)
                    else
                        todo.accumulatedPressure +
                                todo.progress.rightEta * todo.pressurePerMinute +
                                (todo.progress.leftEta - todo.progress.rightEta) * (todo.pressurePerMinute + m[todo.progress.rightNode]!!.pressure) +
                                (maxMinutes - todo.minutesUsed - todo.progress.leftEta) * (todo.pressurePerMinute + m[todo.progress.rightNode]!!.pressure + m[todo.progress.leftNode]!!.pressure)
            }

    }
}

fun <E> List<E>.isPrefixOf(other: List<E>): Boolean =
    this.size <= other.size && this.zip(other).all { it.first == it.second }

private fun Progress.nodes(): Set<String> =
    when (this) {
        BothDone -> setOf()
        is LeftGoingRightDone -> setOf(this.leftNode)
        is LeftDoneRightGoing -> setOf(this.rightNode)
        is BothGoing -> setOf(this.leftNode, this.rightNode)
    }


fun <A> Iterable<A>.pairings(): Iterable<Pair<A,A>> =
    this.times(this).filter { (a,b) -> a != b }

inline fun <T> List<T>.fold1(operation: (acc: T, T) -> T): T? =
    if (this.isEmpty()) null
    else this.drop(1).fold(this.first(), operation)

fun <K,V> List<Map<K,V>>.unionAll(): Map<K,V> =
    this.fold1 { a,b -> a + b } ?: mapOf()
