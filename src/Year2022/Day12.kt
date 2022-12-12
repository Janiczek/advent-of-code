import java.util.PriorityQueue

class Day12 {
    companion object {
        const val inputFile = "input202212.txt"

        fun parse1(input: String): Input = parseGrid(input)
        fun parse2(input: String) = parse1(input)
        fun compute1(input: Input): Int {
            val costs = dijkstra(input, input.start)
            return costs[input.end.first][input.end.second]
        }

        fun compute2(input: Input): Int {
            return findAll(1, input.grid).minOf {
                val costs = dijkstra(input, it)
                costs[input.end.first][input.end.second]
            }
        }

        private fun dijkstra(input: Input, init: Pair<Int,Int>): List<List<Int>> {
            val height = input.grid.size
            val width = input.grid[0].size
            val costs: MutableList<MutableList<Int>> = MutableList(width) { MutableList(height) { Int.MAX_VALUE } }
            val visited: MutableSet<Pair<Int,Int>> = mutableSetOf()
            val queue = PriorityQueue<TodoCell>()
            queue.add(TodoCell(pos = init, cost = 0))
            costs[init.first][init.second] = 0
            while (!visited.contains(input.end) && queue.isNotEmpty()) {
                val cell = queue.poll()
                val (curX,curY) = cell.pos
                if (visited.contains(cell.pos)) continue
                visited.add(cell.pos)
                listOf(-1 to 0, 1 to 0, 0 to -1, 0 to 1)
                    .map { addPos(cell.pos, it) }
                    .filter { isInBounds(it, width, height) && !visited.contains(it) }
                    .forEach {
                        val (newX,newY) = it
                        val curHeight = input.grid[curY][curX]
                        val newHeight = input.grid[newY][newX]
                        if (newHeight - curHeight <= 1) {
                            val newCost = minOf(
                                costs[newX][newY],
                                costs[curX][curY] + 1,
                            )
                            costs[newX][newY] = newCost
                            queue.add(TodoCell(pos = it, cost = newCost))
                        }
                    }
            }
            return costs.map{ it.toList() }.toList()
        }

        private fun isInBounds(pos: Pair<Int, Int>, width: Int, height: Int): Boolean =
            (pos.first in 0 until width) && (pos.second in 0 until height)

        private fun addPos(pos: Pair<Int, Int>, delta: Pair<Int, Int>) =
            pos.first + delta.first to pos.second + delta.second

        data class TodoCell(
            val pos: Pair<Int,Int>,
            val cost: Int,
        ) : Comparable<TodoCell> {
            override fun compareTo(other: TodoCell): Int =
                compareValuesBy(this, other) { it.cost }
        }

        data class Input(
            val grid: List<List<Int>>,
            val start: Pair<Int,Int>,
            val end: Pair<Int,Int>,
        )

        private fun parseGrid(input: String): Input {
            val grid =
                input.lines()
                    .map { line ->
                        line.map { char ->
                            when (char) {
                                'S' -> 0
                                'E' -> 27
                                else -> char - 'a' + 1
                            }
                        }
                    }

            val start = find(0,grid)
            val end = find(27,grid)

            val fixedGrid = grid.map { it.map { when (it) {
                0 -> 1
                27 -> 26
                else -> it
            }}}

            return Input(fixedGrid,start,end)
        }

        private fun findAll(n: Int, grid: List<List<Int>>): List<Pair<Int, Int>> =
            grid.flatMapIndexed { y, row ->
                row.flatMapIndexed { x, it ->
                    if (it == n) listOf(x to y) else listOf()
                }
            }

        private fun find(n: Int, grid: List<List<Int>>): Pair<Int,Int> =
            findAll(n, grid).first()
    }
}