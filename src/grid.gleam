import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/string

pub type XY =
  #(Int, Int)

pub fn xy_add(a: XY, b: XY) -> XY {
  #(a.0 + b.0, a.1 + b.1)
}

pub fn xy_scale(xy: XY, k: Int) -> XY {
  #(xy.0 * k, xy.1 * k)
}

pub type Dims {
  Dims(width: Int, height: Int)
}

pub type Grid(a) {
  Grid(data: Dict(XY, a), dims: Dims)
}

pub type Dir {
  Top
  TopRight
  Right
  BottomRight
  Bottom
  BottomLeft
  Left
  TopLeft
}

pub const orthogonal_dirs = [Top, Right, Bottom, Left]

pub const diagonal_dirs = [TopRight, BottomRight, BottomLeft, TopLeft]

pub const all_dirs = [
  Top,
  TopRight,
  Right,
  BottomRight,
  Bottom,
  BottomLeft,
  Left,
  TopLeft,
]

pub fn from_string(input: String) -> Grid(String) {
  let rows = string.split(input, "\n")
  let grid =
    rows
    |> list.index_map(fn(line, y) {
      line
      |> string.to_graphemes
      |> list.index_map(fn(cell, x) { #(#(x, y), cell) })
    })
    |> list.flatten
    |> dict.from_list
  let width = list.length(rows)
  let height =
    rows
    |> list.first
    |> result.map(string.to_graphemes)
    |> result.unwrap([])
    |> list.length
  let dims = Dims(width, height)
  Grid(grid, dims)
}

pub fn map(grid: Grid(a), fun: fn(XY, a) -> b) -> Grid(b) {
  Grid(dims: grid.dims, data: grid.data |> dict.map_values(fun))
}

pub fn values(grid: Grid(a)) -> List(a) {
  dict.values(grid.data)
}

pub fn in_grid(grid: Grid(a), xy: XY) -> Bool {
  xy.0 >= 0 && xy.1 >= 0 && xy.0 < grid.dims.width && xy.1 < grid.dims.height
}

pub fn delta(dir: Dir) -> XY {
  case dir {
    Top -> #(0, -1)
    TopRight -> #(1, -1)
    Right -> #(1, 0)
    BottomRight -> #(1, 1)
    Bottom -> #(0, 1)
    BottomLeft -> #(-1, 1)
    Left -> #(-1, 0)
    TopLeft -> #(-1, -1)
  }
}

pub fn deltas(dir: Dir, count: Int) -> List(XY) {
  let delta_ = delta(dir)

  list.range(0, count - 1)
  |> list.map(fn(i) { xy_scale(delta_, i) })
}

/// Returns Err if it hits a boundary
pub fn in_dir(
  grid: Grid(a),
  start: XY,
  dir: Dir,
  count: Int,
) -> Result(List(XY), Nil) {
  let coords =
    deltas(dir, count)
    |> list.map(fn(d) { xy_add(start, d) })
  use <- bool.guard(
    when: list.any(coords, fn(xy) { !in_grid(grid, xy) }),
    return: Error(Nil),
  )
  Ok(coords)
}

/// Always returns a list, even if incomplete
pub fn in_dir_greedy(start: XY, dir: Dir, count: Int) -> List(XY) {
  deltas(dir, count)
  |> list.map(fn(d) { xy_add(start, d) })
}

/// Always returns a list, wraps around to the other side if it hits a boundary
pub fn in_dir_wraparound(
  grid: Grid(a),
  start: XY,
  dir: Dir,
  count: Int,
) -> List(XY) {
  deltas(dir, count)
  |> list.map(fn(d) { xy_add(start, d) |> wraparound(grid.dims) })
}

pub fn wraparound(xy: XY, dims: Dims) -> XY {
  xy
  |> wraparound_x(dims)
  |> wraparound_y(dims)
}

pub fn wraparound_x(xy: XY, dims: Dims) -> XY {
  case xy.0 < 0 {
    True -> wraparound_x(#(xy.0 + dims.width, xy.1), dims)
    False ->
      case xy.0 >= dims.width {
        True -> wraparound_x(#(xy.0 - dims.width, xy.1), dims)
        False -> xy
      }
  }
}

pub fn wraparound_y(xy: XY, dims: Dims) -> XY {
  case xy.1 < 0 {
    True -> wraparound_y(#(xy.0, xy.1 + dims.height), dims)
    False ->
      case xy.1 >= dims.height {
        True -> wraparound_y(#(xy.0, xy.1 - dims.height), dims)
        False -> xy
      }
  }
}

pub fn get(grid: Grid(a), xy: XY) -> Result(a, Nil) {
  dict.get(grid.data, xy)
}

pub fn get_all(grid: Grid(a), xys: List(XY)) -> Result(List(a), Nil) {
  list.try_map(xys, fn(xy) { dict.get(grid.data, xy) })
}
