import gleam/dict.{type Dict}
import gleam/list
import gleam/otp/task
import gleam/result

/// Needs the operation to be associative and there to be a zero (monoid).
/// Uses parallelism under the hood.
pub fn smush(xs: List(x), zero: x, fun: fn(x, x) -> x) -> x {
  smush_aux(xs, list.length(xs), zero, fun)
}

fn smush_aux(xs: List(x), length: Int, zero: x, fun: fn(x, x) -> x) -> x {
  case xs {
    [] -> zero
    [x] -> x
    _ -> {
      let half = length / 2
      let #(left, right) = list.split(xs, at: half)
      let left_handle = task.async(fn() { smush_aux(left, half, zero, fun) })
      let right_result = smush_aux(right, length - half, zero, fun)
      let left_result = task.await_forever(left_handle)
      fun(left_result, right_result)
    }
  }
}

pub fn pmap(xs: List(x), with fun: fn(x) -> y) -> List(y) {
  xs
  |> list.map(fn(x) { task.async(fn() { fun(x) }) })
  |> list.map(task.await_forever)
}

pub fn pmap2(xs: List(x), ys: List(y), with fun: fn(x, y) -> z) -> List(z) {
  list.map2(xs, ys, fn(x, y) { task.async(fn() { fun(x, y) }) })
  |> list.map(task.await_forever)
}

/// frequencies([5,10,2,5,10,5]) -> dict.from_list([ #(2,1), #(5,3), #(10,2) ])
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

/// consecutive_pairs([5,10,2]) -> [ #(5,10), #(10,2) ]
pub fn consecutive_pairs(xs: List(a)) -> List(#(a, a)) {
  list.map2(xs, list.drop(xs, 1), fn(x, y) { #(x, y) })
}

/// remove_at([5,10,2],1) -> [5,2]
pub fn remove_at(xs: List(a), at i: Int) -> List(a) {
  list.append(list.take(xs, i), list.drop(xs, i + 1))
}

/// selections([5,10,2]) -> [ #(5,[10,2]), #(10,[5,2]), #(2,[5,10]) ]
pub fn selections(xs: List(a)) -> List(#(a, List(a))) {
  list.index_map(xs, fn(x, i) { #(x, remove_at(xs, i)) })
}
