import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string
import gleamy/priority_queue.{type Queue}

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

pub fn xy_mod(a: XY, m: XY) -> XY {
  #(a.0 % m.0, a.1 % m.1)
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

pub const empty_dims: Dims = Dims(
  width: 0,
  height: 0,
  min_x: 0,
  max_x: 0,
  min_y: 0,
  max_y: 0,
)

pub fn from_list(items: List(#(XY, a))) -> Grid(a) {
  items
  |> list.fold(
    from: Grid(dims: empty_dims, data: dict.new()),
    with: fn(acc, kv) { insert(acc, kv.0, kv.1) },
  )
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

pub fn update(grid: Grid(a), at xy: XY, fun fun: fn(a) -> a) -> Grid(a) {
  case dict.get(grid.data, xy) {
    Error(Nil) -> grid
    Ok(a) -> Grid(..grid, data: dict.insert(grid.data, xy, fun(a)))
  }
}

pub fn delete(grid: Grid(a), at xy: XY) -> Grid(a) {
  Grid(..grid, data: dict.delete(grid.data, xy))
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

pub fn keys(grid: Grid(a)) -> List(XY) {
  dict.keys(grid.data)
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

/// Doesn't expand the grid if the `xy` is outside the grid. Just ignores it.
pub fn set(grid: Grid(a), xy: XY, value: a) -> Grid(a) {
  case in_grid(grid, xy) {
    True -> Grid(..grid, data: dict.insert(grid.data, xy, value))
    False -> grid
  }
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

/// Will only report neighbours _inside_ the grid
pub fn neighbours(
  grid: Grid(a),
  xy: XY,
  dirs: List(Dir),
) -> List(#(XY, Result(a, Nil))) {
  raw_neighbours(xy, dirs)
  |> list.filter_map(fn(neighbour) {
    case in_grid(grid, neighbour) {
      False -> Error(Nil)
      True -> {
        let item = get(grid, neighbour)
        Ok(#(neighbour, item))
      }
    }
  })
}

pub fn are_neighbours(a: XY, b: XY, dirs: List(Dir)) -> Bool {
  list.any(dirs, fn(dir) { step(a, dir, 1) == b })
}

pub fn raw_neighbours(xy: XY, dirs: List(Dir)) -> List(XY) {
  list.map(dirs, fn(dir) { step(xy, dir, 1) })
}

/// Dijkstra BFS. Maybe later we'll do A* instead.
pub fn shortest_path(
  from origin: XY,
  to goal: XY,
  neighbours neighbours: fn(XY) -> List(XY),
) -> Result(#(List(XY), Int), Nil) {
  shortest_path_aux(
    goal: goal,
    seen: dict.new(),
    todos: priority_queue.from_list(
      [ShortestPathTodo(origin, [], 0)],
      compare_todo,
    ),
    neighbours: neighbours,
  )
}

type ShortestPathTodo {
  ShortestPathTodo(current: XY, steps_rev: List(XY), steps_count: Int)
}

fn shortest_path_aux(
  goal goal: XY,
  seen seen: Dict(XY, Int),
  todos todos: Queue(ShortestPathTodo),
  neighbours neighbours: fn(XY) -> List(XY),
) -> Result(#(List(XY), Int), Nil) {
  case priority_queue.pop(todos) {
    Error(Nil) -> Error(Nil)
    Ok(#(todo_, rest_of_todos)) -> {
      use <- bool.guard(
        when: todo_.current == goal,
        return: Ok(#(list.reverse(todo_.steps_rev), todo_.steps_count)),
      )
      // We're guaranteed by filtering from previous steps that this path will be the best seen for the XY:
      let new_seen = dict.insert(seen, todo_.current, todo_.steps_count)
      let new_neighbours =
        neighbours(todo_.current)
        |> list.filter(fn(neighbour) {
          case dict.get(seen, neighbour) {
            Error(Nil) -> True
            Ok(previously_best_count) ->
              previously_best_count > todo_.steps_count + 1
          }
        })
      let added_todos =
        new_neighbours
        |> list.map(fn(neighbour) {
          ShortestPathTodo(
            current: neighbour,
            steps_rev: [neighbour, ..todo_.steps_rev],
            steps_count: todo_.steps_count + 1,
          )
        })
      let new_todos =
        list.fold(
          over: added_todos,
          from: rest_of_todos,
          with: priority_queue.push,
        )
      shortest_path_aux(goal, new_seen, new_todos, neighbours)
    }
  }
}

fn compare_todo(a: ShortestPathTodo, b: ShortestPathTodo) {
  int.compare(a.steps_count, b.steps_count)
}

/// TODO: doesn't do anything about the data outside the new dims.
pub fn with_dims(grid: Grid(a), dims: Dims) -> Grid(a) {
  Grid(dims: dims, data: grid.data)
}

/// Will silently do nothing if there's nothing to move
pub fn move(grid: Grid(a), from origin: XY, to target: XY) {
  case dict.get(grid.data, origin) {
    Error(Nil) -> grid
    Ok(a) ->
      Grid(
        ..grid,
        data: grid.data
          |> dict.delete(origin)
          |> dict.insert(target, a),
      )
  }
}

pub fn to_string(
  grid grid: Grid(a),
  fun fun: fn(a) -> #(String, Result(String, Nil)),
  empty empty: String,
) -> String {
  let x_range = list.range(from: grid.dims.min_x, to: grid.dims.max_x)
  list.range(from: grid.dims.min_y, to: grid.dims.max_y)
  |> list.map(fn(y) {
    let #(ascii, metadata) =
      x_range
      |> list.map(fn(x) {
        case get(grid, #(x, y)) {
          Error(Nil) -> #(empty, Error(Nil))
          Ok(a) -> fun(a)
        }
      })
      |> list.unzip

    [
      ascii |> string.concat,
      metadata
        |> result.values
        |> string.join(", "),
    ]
    |> string.join("  ")
  })
  |> string.join("\n")
}

pub fn find_all_exact(grid: Grid(a), a: a) -> List(XY) {
  grid.data
  |> dict.to_list()
  |> list.filter(fn(kv) { kv.1 == a })
  |> list.map(fn(kv) { kv.0 })
}

pub fn group_consecutive(grid: Grid(a), dirs: List(Dir)) -> List(#(a, List(XY))) {
  grid.data
  |> dict.to_list
  |> list.group(by: fn(kv) { kv.1 })
  |> dict.map_values(fn(a, group) {
    let todos = list.map(group, fn(kv) { kv.0 })
    group_consecutive_split(dirs, todos, [])
    |> list.map(fn(group) { #(a, group) })
  })
  |> dict.values
  |> list.flatten
}

/// In principle: [(1,1),(1,0),(9,9)] -> [[(1,1),(1,0)], [(9,9)]]
///    [],                       [(1,1),(1,0),(9,9)]
/// -> [[(1,1)]],                [(1,0),(9,9)]
/// -> [[(1,0),(1,1)]],          [(9,9)]
/// -> [[(1,0),(1,1)], [(9,9)]], []
/// This could be done with some kind of union-find structure, but I didn't feel
/// like implementing that.
fn group_consecutive_split(
  dirs: List(Dir),
  todos: List(XY),
  acc_groups: List(Set(XY)),
) -> List(List(XY)) {
  case todos {
    [] ->
      acc_groups
      |> group_consecutive_fixup(dirs)
      |> list.map(set.to_list)
    [todo_, ..rest] -> {
      let neighbours_ = raw_neighbours(todo_, dirs)

      let result =
        acc_groups
        |> list.fold(#(False, []), fn(new_acc, group) {
          use <- bool.guard(
            when: new_acc.0,
            return: #(new_acc.0, [group, ..new_acc.1]),
          )
          case
            list.any(neighbours_, fn(neighbour) {
              set.contains(group, neighbour)
            })
          {
            True -> #(True, [set.insert(group, todo_), ..new_acc.1])
            False -> #(new_acc.0, [group, ..new_acc.1])
          }
        })

      // If we didn't find the right group to insert into, let's make a new group
      let new_acc = case result.0 {
        True -> result.1
        False -> [set.from_list([todo_]), ..result.1]
      }

      group_consecutive_split(dirs, rest, new_acc)
    }
  }
}

fn group_consecutive_fixup(
  groups: List(Set(XY)),
  dirs: List(Dir),
) -> List(Set(XY)) {
  let result =
    groups
    |> list.combination_pairs
    |> list.find_map(fn(combination) {
      let group_1_neighbours =
        combination.0
        |> set.to_list
        |> list.flat_map(raw_neighbours(_, dirs))
        |> set.from_list
      let group_2 = combination.1

      case set.is_empty(set.intersection(group_1_neighbours, group_2)) {
        True -> Error(Nil)
        False -> {
          let combined_group = set.union(combination.0, combination.1)
          let groups_without_combined =
            groups
            |> list.filter(fn(group) {
              group != combination.0 && group != combination.1
            })
          Ok([combined_group, ..groups_without_combined])
        }
      }
    })

  case result {
    Error(Nil) -> groups
    Ok(new_groups) -> group_consecutive_fixup(new_groups, dirs)
  }
}

pub fn count(grid: Grid(a), pred: fn(XY, a) -> Bool) -> Int {
  grid.data
  |> dict.filter(pred)
  |> dict.size
}

pub fn extend_edge(grid: Grid(a), with value: a) -> Grid(a) {
  //        #####
  // 111    #111#
  // 121 -> #121#
  // 111    #111#
  //        #####
  let xl = grid.dims.min_x - 1
  let xr = grid.dims.max_x + 1
  let yt = grid.dims.min_y - 1
  let yb = grid.dims.max_y + 1
  let x_range = list.range(xl, xr)
  let y_range = list.range(grid.dims.min_y, grid.dims.max_y)
  let edges: Dict(XY, a) =
    [
      x_range |> list.map(fn(x) { #(#(x, yt), value) }),
      x_range |> list.map(fn(x) { #(#(x, yb), value) }),
      y_range |> list.map(fn(y) { #(#(xl, y), value) }),
      y_range |> list.map(fn(y) { #(#(xr, y), value) }),
    ]
    |> list.flatten
    |> dict.from_list
  Grid(dims: extend_edge_dims(grid.dims), data: dict.merge(grid.data, edges))
}

fn extend_edge_dims(dims: Dims) -> Dims {
  Dims(
    min_x: dims.min_x - 1,
    max_x: dims.max_x + 1,
    min_y: dims.min_y - 1,
    max_y: dims.max_y + 1,
    width: dims.width + 2,
    height: dims.height + 2,
  )
}

pub fn remove_bottom_row(grid: Grid(a)) -> Grid(a) {
  Grid(
    dims: Dims(
      ..grid.dims,
      max_y: grid.dims.max_y - 1,
      height: grid.dims.height - 1,
    ),
    data: grid.data
      |> dict.filter(fn(xy, _) { xy.1 != grid.dims.max_y }),
  )
}

pub fn remove_right_column(grid: Grid(a)) -> Grid(a) {
  Grid(
    dims: Dims(
      ..grid.dims,
      max_x: grid.dims.max_x - 1,
      width: grid.dims.width - 1,
    ),
    data: grid.data
      |> dict.filter(fn(xy, _) { xy.0 != grid.dims.max_x }),
  )
}
