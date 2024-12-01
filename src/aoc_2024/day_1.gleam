import extra
import gleam/dict
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type Input =
  #(List(Int), List(Int))

pub fn parse(input: String) -> Input {
  input
  |> string.split("\n")
  |> list.map(string.split(_, "   "))
  |> list.fold(from: #([], []), with: fn(acc, row) {
    let #(xs, ys) = acc
    case row {
      [x, y] -> {
        let assert Ok(xx) = int.parse(x)
        let assert Ok(yy) = int.parse(y)
        #([xx, ..xs], [yy, ..ys])
      }
      _ -> panic as "Bad input"
    }
  })
}

pub fn pt_1(input: Input) {
  let #(xs, ys) = input
  let sorted_xs = xs |> list.sort(by: int.compare)
  let sorted_ys = ys |> list.sort(by: int.compare)
  list.map2(sorted_xs, sorted_ys, with: fn(x, y) { int.absolute_value(x - y) })
  |> int.sum
}

pub fn pt_2(input: Input) {
  let #(xs, ys) = input
  let freqs = extra.frequencies(ys)

  xs
  |> list.map(fn(x) { x * { dict.get(freqs, x) |> result.unwrap(0) } })
  |> int.sum
}
