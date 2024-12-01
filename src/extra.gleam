import gleam/dict.{type Dict}
import gleam/list
import gleam/result

pub fn frequencies(xs: List(a)) -> Dict(a, Int) {
  xs
  |> list.fold(from: dict.new(), with: fn(counter, x) {
    let old: Int =
      counter
      |> dict.get(x)
      |> result.unwrap(0)

    counter
    |> dict.insert(x, old + 1)
  })
}
