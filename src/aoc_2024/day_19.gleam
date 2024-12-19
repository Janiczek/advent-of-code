import gleam/bool
import gleam/int
import gleam/list
import gleam/string
import pocket_watch
import rememo/memo

pub type Input {
  Input(towels: List(String), patterns: List(String))
}

pub fn parse(input: String) -> Input {
  use <- pocket_watch.simple("parse")
  case string.split(input, "\n\n") {
    [towels_str, patterns_str] -> {
      Input(
        towels: string.split(towels_str, ", "),
        patterns: string.split(patterns_str, "\n"),
      )
    }
    _ -> panic as "Bad input"
  }
}

pub fn pt_1(input: Input) {
  use <- pocket_watch.simple("part 1")
  input.patterns
  |> list.count(pt_1_possible(input.towels, _))
}

fn pt_1_possible(towels: List(String), pattern: String) -> Bool {
  pt_1_possible_aux(towels, [pattern])
}

fn pt_1_possible_aux(towels: List(String), todos: List(String)) -> Bool {
  case todos {
    [] -> False
    [todo_, ..rest] -> {
      use <- bool.guard(
        when: list.any(towels, fn(towel) { towel == todo_ }),
        return: True,
      )

      let next_towels: List(String) =
        towels
        |> list.filter(fn(towel) { string.starts_with(todo_, towel) })
      let added_todos: List(String) =
        next_towels
        |> list.map(fn(towel) { string.drop_start(todo_, string.length(towel)) })
      pt_1_possible_aux(towels, list.append(added_todos, rest))
    }
  }
}

pub fn pt_2(input: Input) {
  use <- pocket_watch.simple("part 2")
  use cache <- memo.create()
  input.patterns
  |> list.map(pt_2_all(input.towels, _, cache))
  |> int.sum
}

fn pt_2_all(towels: List(String), pattern: String, cache) -> Int {
  use <- memo.memoize(cache, #(towels, pattern))
  let #(done, not_done) = list.partition(towels, fn(towel) { towel == pattern })
  let done_towels = list.length(done)
  let rec =
    not_done
    |> list.filter(fn(towel) { string.starts_with(pattern, towel) })
    |> list.map(fn(towel) { string.drop_start(pattern, string.length(towel)) })
    |> list.map(fn(rest) { pt_2_all(towels, rest, cache) })
    |> int.sum
  done_towels + rec
}
