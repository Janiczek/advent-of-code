import extra
import gleam/bool
import gleam/int
import gleam/list
import gleam/string
import pocket_watch
import rememo/memo

pub fn parse(input: String) -> List(Int) {
  use <- pocket_watch.simple("parse")
  input
  |> string.split(" ")
  |> list.map(extra.yolo_int)
}

pub fn pt_1(input: List(Int)) {
  use <- pocket_watch.simple("pt_1")
  use cache <- memo.create()
  input
  |> list.map(c(_, 25, cache))
  |> int.sum
}

pub fn pt_2(input: List(Int)) {
  use <- pocket_watch.simple("pt_2")
  use cache <- memo.create()
  input
  |> list.map(c(_, 75, cache))
  |> int.sum
}

fn c(n: Int, i: Int, cache) -> Int {
  use <- memo.memoize(cache, #(n, i))
  use <- bool.guard(when: i == 0, return: 1)
  case n {
    0 -> c(1, i - 1, cache)
    _ -> {
      let str = int.to_string(n)
      let len = string.length(str)
      case len % 2 == 0 {
        True -> {
          let half = len / 2
          let left =
            extra.yolo_int(string.slice(str, at_index: 0, length: half))
          let right =
            extra.yolo_int(string.slice(str, at_index: half, length: half))
          c(left, i - 1, cache) + c(right, i - 1, cache)
        }
        False -> c(n * 2024, i - 1, cache)
      }
    }
  }
}
