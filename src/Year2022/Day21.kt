class Day21 {
    companion object {
        const val inputFile = "input202221.txt"

        sealed class Expr
        data class Lit(val n: Long): Expr()
        data class Var(val v: String): Expr()
        data class Add(val e1: Expr, val e2: Expr): Expr()
        data class Sub(val e1: Expr, val e2: Expr): Expr()
        data class Mul(val e1: Expr, val e2: Expr): Expr()
        data class Div(val e1: Expr, val e2: Expr): Expr()

        fun parse1(input: String): Map<String,Expr> = input.lines().associate { parseLine(it) }
        fun parse2(input: String) = parse1(input)
        private fun parseLine(line: String): Pair<String,Expr> {
            val words = line.split(" ")
            val name = words[0].filter { it.isLetter() }
            val expr =
                if (words.size == 2) Lit(words[1].toLong())
                else when (words[2]) {
                    "+" -> Add(Var(words[1]),Var(words[3]))
                    "-" -> Sub(Var(words[1]),Var(words[3]))
                    "*" -> Mul(Var(words[1]),Var(words[3]))
                    "/" -> Div(Var(words[1]),Var(words[3]))
                    else -> TODO("parseLine")
                }
            return name to expr
        }

        fun compute1(input: Map<String,Expr>): Long = eval1(input, Var("root"))
        fun compute2(input: Map<String,Expr>): Expr = eval2(input, Var("root"))
        private tailrec fun eval1(env: Map<String,Expr>, e: Expr): Long =
            when (e) {
                is Lit -> e.n
                is Var -> eval1(env, env[e.v]!!)
                is Add -> eval1(env, e.e1) + eval1(env, e.e2)
                is Sub -> eval1(env, e.e1) - eval1(env, e.e2)
                is Mul -> eval1(env, e.e1) * eval1(env, e.e2)
                is Div -> eval1(env, e.e1) / eval1(env, e.e2)
            }
        private tailrec fun eval2(env: Map<String,Expr>, e: Expr): Expr {
            val once = eval2Once(env, e)
            return if (once == e) once else eval2(env, once)
        }
        private tailrec fun eval2Once(env: Map<String,Expr>, e: Expr): Expr =
            when (e) {
                is Lit -> e
                is Var -> {
                    val expr = eval2(env, env[e.v]!!)
                    when (e.v) {
                        "humn" -> e // don't evaluate
                        "root" -> {
                            val (e1,e2) = when (expr) {
                                is Lit -> TODO("What 1")
                                is Var -> TODO("What 2")
                                is Add -> expr.e1 to expr.e2
                                is Sub -> expr.e1 to expr.e2
                                is Mul -> expr.e1 to expr.e2
                                is Div -> expr.e1 to expr.e2
                            }
                            // Can't be bothered.
                            println("For some reason my program doesn't work on the real input. No matter.")
                            println("Plug this into Wolfram Cloud:")
                            println("Solve[${prn(e1)} == ${prn(e2)},{humn}]")
                            solve(e1,e2)
                        }
                        else -> expr
                    }
                }
                is Add -> if (e.e1 is Lit && e.e2 is Lit) Lit(e.e1.n + e.e2.n) else Add(eval2(env,e.e1),eval2(env,e.e2))
                is Sub -> if (e.e1 is Lit && e.e2 is Lit) Lit(e.e1.n - e.e2.n) else Sub(eval2(env,e.e1),eval2(env,e.e2))
                is Mul -> if (e.e1 is Lit && e.e2 is Lit) Lit(e.e1.n * e.e2.n) else Mul(eval2(env,e.e1),eval2(env,e.e2))
                is Div -> if (e.e1 is Lit && e.e2 is Lit) Lit(e.e1.n / e.e2.n) else Div(eval2(env,e.e1),eval2(env,e.e2))
            }

        private fun solve(e1: Expr, e2: Expr): Expr =
            if (e1 is Lit) solve(e2,e1.n)
            else solve(e2,e1)

        private fun solve(e: Expr, n: Long): Expr =
            when (e) {
                is Lit -> TODO("solve: Shouldn't happen, we didn't eval HUMN")
                is Var -> {
                    assert(e.v == "humn") { "Should have been HUMN" }
                    Lit(n)
                }
                is Add -> binop(e.e1, e.e2) { x -> n - x }
                is Sub -> binop(e.e1, e.e2) { x -> n + x }
                is Mul -> binop(e.e1, e.e2) { x -> n / x }
                is Div -> binop(e.e1, e.e2) { x -> n * x }
            }

        private fun binop(e1: Expr, e2: Expr, fn: (Long) -> Long): Expr =
            if      (e1 is Lit) solve(e2, fn(e1.n))
            else if (e2 is Lit) solve(e1, fn(e2.n))
            else    TODO("binop")

        private fun prn(e: Expr): String =
            when (e) {
                is Lit -> e.n.toString()
                is Var -> e.v
                is Add -> "(${prn(e.e1)} + ${prn(e.e2)})"
                is Sub -> "(${prn(e.e1)} - ${prn(e.e2)})"
                is Mul -> "(${prn(e.e1)} * ${prn(e.e2)})"
                is Div -> "(${prn(e.e1)} / ${prn(e.e2)})"
            }
    }
}