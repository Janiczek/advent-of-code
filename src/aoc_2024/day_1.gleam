import extra
import gleam/dict
import gleam/int
import gleam/list
import gleam/otp/task
import gleam/result
import gleam/string
import pocket_watch

pub type Input =
  #(List(Int), List(Int))

pub fn parse(input: String) -> Input {
  use <- pocket_watch.simple("parse")
  input
  |> string.split("\n")
  |> extra.pmap(string.split(_, "   "))
  |> list.fold(from: #([], []), with: fn(acc, row) {
    let #(xs, ys) = acc
    case row {
      [x, y] -> {
        #([extra.yolo_int(x), ..xs], [extra.yolo_int(y), ..ys])
      }
      _ -> panic as "Bad input"
    }
  })
}

pub fn pt_1(input: Input) {
  use <- pocket_watch.simple("part 1")
  let sorted_xs_handle = task.async(fn() { list.sort(input.0, int.compare) })
  let sorted_ys_handle = task.async(fn() { list.sort(input.1, int.compare) })
  let sorted_xs = task.await_forever(sorted_xs_handle)
  let sorted_ys = task.await_forever(sorted_ys_handle)

  extra.pmap2(sorted_xs, sorted_ys, fn(x, y) { int.absolute_value(x - y) })
  |> int.sum
}

pub fn pt_2(input: Input) {
  use <- pocket_watch.simple("part 2")
  let #(xs, ys) = input
  let freqs = extra.frequencies(ys)

  xs
  |> extra.pmap(fn(x) { x * { dict.get(freqs, x) |> result.unwrap(0) } })
  |> int.sum
}
