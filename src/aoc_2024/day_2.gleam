import extra
import gleam/int
import gleam/list
import gleam/string
import pocket_watch

pub fn parse(input: String) -> List(List(Int)) {
  use <- pocket_watch.simple("parse")
  input
  |> string.split("\n")
  |> list.map(fn(line) {
    line
    |> string.split(" ")
    |> list.map(extra.yolo_int)
  })
}

fn is_ascending(line: List(Int)) {
  line == list.sort(line, by: int.compare)
}

fn is_descending(line: List(Int)) {
  list.reverse(line) == list.sort(line, by: int.compare)
}

fn has_good_differences(line: List(Int)) {
  list.window_by_2(line)
  |> list.map(fn(pair) { int.absolute_value(pair.0 - pair.1) })
  |> list.all(fn(n) { n >= 1 && n <= 3 })
}

fn is_ok(line: List(Int)) {
  { is_ascending(line) || is_descending(line) } && has_good_differences(line)
}

pub fn pt_1(input: List(List(Int))) {
  use <- pocket_watch.simple("part 1")
  list.count(input, is_ok)
}

pub fn pt_2(input: List(List(Int))) {
  use <- pocket_watch.simple("part 2")
  input
  |> list.count(fn(line) {
    line
    |> list.combinations(list.length(line) - 1)
    |> list.any(is_ok)
  })
}
