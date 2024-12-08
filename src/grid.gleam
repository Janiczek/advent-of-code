import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type XY =
  #(Int, Int)

pub fn xy_add(a: XY, b: XY) -> XY {
  #(a.0 + b.0, a.1 + b.1)
}

pub fn xy_sub(a: XY, b: XY) -> XY {
  #(a.0 - b.0, a.1 - b.1)
}

pub fn xy_scale(xy: XY, k: Int) -> XY {
  #(xy.0 * k, xy.1 * k)
}

pub type Dims {
  Dims(min_x: Int, max_x: Int, min_y: Int, max_y: Int, width: Int, height: Int)
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

pub fn turn_90deg_right(dir: Dir) -> Dir {
  case dir {
    Top -> Right
    TopRight -> BottomRight
    Right -> Bottom
    BottomRight -> BottomLeft
    Bottom -> Left
    BottomLeft -> TopLeft
    Left -> Top
    TopLeft -> TopRight
  }
}

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
  let dims =
    Dims(
      min_x: 0,
      max_x: width - 1,
      min_y: 0,
      max_y: height - 1,
      width: width,
      height: height,
    )
  Grid(grid, dims)
}

pub fn map(grid: Grid(a), fun: fn(XY, a) -> b) -> Grid(b) {
  Grid(dims: grid.dims, data: grid.data |> dict.map_values(fun))
}

pub fn filter(grid: Grid(a), pred: fn(XY, a) -> Bool) -> Grid(a) {
  Grid(..grid, data: grid.data |> dict.filter(pred))
}

pub fn filter_map(grid: Grid(a), fun: fn(XY, a) -> Result(b, Nil)) -> Grid(b) {
  Grid(
    dims: grid.dims,
    data: grid.data
      |> dict.to_list
      |> list.filter_map(fn(kv) {
        case fun(kv.0, kv.1) {
          Ok(v2) -> Ok(#(kv.0, v2))
          Error(Nil) -> Error(Nil)
        }
      })
      |> dict.from_list,
  )
}

pub fn find_exact(grid: Grid(a), needle: a) -> Result(XY, Nil) {
  grid.data
  |> dict.to_list
  |> list.find_map(fn(kv) {
    case kv.1 == needle {
      True -> Ok(kv.0)
      False -> Error(Nil)
    }
  })
}

pub fn to_list(grid: Grid(a)) -> List(#(XY, a)) {
  dict.to_list(grid.data)
}

pub fn values(grid: Grid(a)) -> List(a) {
  dict.values(grid.data)
}

pub fn in_grid(grid: Grid(a), xy: XY) -> Bool {
  in_dims(grid.dims, xy)
}

pub fn in_dims(dims: Dims, xy: XY) -> Bool {
  xy.0 >= dims.min_x
  && xy.0 <= dims.max_x
  && xy.1 >= dims.min_y
  && xy.1 <= dims.max_y
}

pub fn extend(dims: Dims, xy: XY) -> Dims {
  let new_min_x = int.min(dims.min_x, xy.0)
  let new_max_x = int.max(dims.max_x, xy.0)
  let new_min_y = int.min(dims.min_y, xy.1)
  let new_max_y = int.max(dims.max_y, xy.1)
  Dims(
    min_x: new_min_x,
    max_x: new_max_x,
    min_y: new_min_y,
    max_y: new_max_y,
    width: new_max_x - new_min_x + 1,
    height: new_max_y - new_min_y + 1,
  )
}

/// Expands the grid dimensions if the `xy` is outside the grid.
pub fn insert(grid: Grid(a), xy: XY, value: a) -> Grid(a) {
  Grid(dims: extend(grid.dims, xy), data: dict.insert(grid.data, xy, value))
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

/// Always returns a list, even if out of bounds
pub fn in_dir_greedy(start: XY, dir: Dir, count: Int) -> List(XY) {
  deltas(dir, count)
  |> list.map(fn(d) { xy_add(start, d) })
}

/// Can get out of bounds.
pub fn step(start: XY, dir: Dir, count: Int) -> XY {
  xy_add(start, xy_scale(delta(dir), count))
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
