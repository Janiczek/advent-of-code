import extra
import gleam/dict
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import grid.{type XY, Grid}

pub fn parse(input: String) -> List(XY) {
  input
  |> string.split("\n")
  |> list.map(fn(line) {
    case string.split(line, ",") {
      [x, y] -> #(extra.yolo_int(x), extra.yolo_int(y))
      _ -> panic as "bad input?"
    }
  })
}

pub fn pt_1(input: List(XY)) {
  let size = 71
  // let size = 7
  let board = grid.dims_of_size(width: size, height: size)
  let first_kb =
    input
    |> list.take(1024)
    // |> list.take(12)
    |> list.filter(grid.in_dims(board, _))
    |> list.map(fn(xy) { #(xy, Nil) })
  let grid = Grid(dims: board, data: dict.new()) |> grid.set_all(first_kb)
  io.println_error(
    grid.to_string_simple(grid, fn(_, c) {
      case c {
        Ok(Nil) -> "#"
        Error(Nil) -> "."
      }
    }),
  )
  let assert Ok(path) =
    grid.shortest_path(
      from: #(0, 0),
      to: #(grid.dims.max_x, grid.dims.max_y),
      neighbours: fn(xy) {
        grid.neighbours(grid, xy, grid.orthogonal_dirs)
        |> list.filter(fn(kv) { grid.in_grid(grid, kv.0) })
        |> list.filter(fn(kv) { kv.1 |> result.is_error })
        |> list.map(fn(kv) { kv.0 })
      },
    )
  path.1
}

pub fn pt_2(input: List(XY)) {
  todo as "part 2 not implemented"
}
