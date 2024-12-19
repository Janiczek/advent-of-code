import extra
import gleam/dict
import gleam/list
import gleam/result
import gleam/string
import grid.{type Dims, type XY, Grid}
import pocket_watch

pub fn parse(input: String) -> List(XY) {
  use <- pocket_watch.simple("parse")
  input
  |> string.split("\n")
  |> list.map(fn(line) {
    case string.split(line, ",") {
      [x, y] -> #(extra.yolo_int(x), extra.yolo_int(y))
      _ -> panic as "bad input?"
    }
  })
}

// example: size 7, take 12
const size = 71

pub fn pt_1(input: List(XY)) -> Int {
  use <- pocket_watch.simple("part 1")
  let board = grid.dims_of_size(width: size, height: size)
  let taken = 1024
  let assert Ok(cost) = attempt(input, board, taken)
  cost
}

pub fn pt_2(input: List(XY)) {
  use <- pocket_watch.simple("part 2")
  let board = grid.dims_of_size(width: size, height: size)
  pt_2_loop(input, board, 1025)
}

fn pt_2_loop(input: List(XY), board: Dims, taken: Int) -> XY {
  case attempt(input, board, taken) {
    Ok(_) -> pt_2_loop(input, board, taken + 1)
    Error(last) -> last
  }
}

fn attempt(input: List(XY), board: Dims, taken: Int) -> Result(Int, XY) {
  let first_kb =
    input
    |> list.take(taken)
    |> list.filter(grid.in_dims(board, _))
    |> list.map(fn(xy) { #(xy, Nil) })
  let grid = Grid(dims: board, data: dict.new()) |> grid.set_all(first_kb)
  let found_cost =
    grid.astar(
      from: #(0, 0),
      to: #(size - 1, size - 1),
      neighbours: fn(xy) {
        grid.neighbours(grid, xy, grid.orthogonal_dirs)
        |> list.filter(fn(kv) { kv.1 |> result.is_error })
        |> list.map(fn(kv) { kv.0 })
      },
      cost: grid.manhattan_distance,
      print_progress: fn(_, _, _) { Nil },
    )
  case found_cost {
    Error(Nil) -> {
      let assert Ok(last) = input |> list.drop(taken - 1) |> list.first
      Error(last)
    }
    Ok(cost) -> Ok(cost)
  }
}
