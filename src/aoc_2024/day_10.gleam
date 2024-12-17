import extra
import gleam/int
import gleam/list
import gleam/set.{type Set}
import grid.{type Grid, type XY}
import pocket_watch

pub fn parse(input: String) -> Grid(Int) {
  use <- pocket_watch.simple("parse")
  grid.from_string(input)
  |> grid.map(fn(_, c) { extra.yolo_int(c) })
}

pub fn pt_1(input: Grid(Int)) {
  use <- pocket_watch.simple("part 1")
  let starting_positions: List(XY) = grid.find_all_exact(input, 0)
  starting_positions
  |> list.map(trailheads_count(input, _))
  |> int.sum
}

pub fn pt_2(input: Grid(Int)) {
  use <- pocket_watch.simple("part 2")
  let starting_positions: List(XY) = grid.find_all_exact(input, 0)
  starting_positions
  |> list.map(trailheads_rating(input, _))
  |> int.sum
}

type Trailhead {
  Trailhead(xy: XY, height: Int)
}

fn trailheads_count(input: Grid(Int), start: XY) -> Int {
  find_trailheads(
    input,
    todos: [Trailhead(xy: start, height: 0)],
    count: 0,
    seen: set.new(),
  )
  |> fn(x) { set.size(x.1) }
}

fn trailheads_rating(input: Grid(Int), start: XY) -> Int {
  find_trailheads(
    input,
    todos: [Trailhead(xy: start, height: 0)],
    count: 0,
    seen: set.new(),
  )
  |> fn(x) { x.0 }
}

fn find_trailheads(
  input: Grid(Int),
  todos todos: List(Trailhead),
  count count: Int,
  seen seen: Set(XY),
) -> #(Int, Set(XY)) {
  case todos {
    [] -> #(count, seen)
    [todo_, ..rest] -> {
      case todo_.height == 9 {
        True ->
          find_trailheads(
            input,
            todos: rest,
            count: count + 1,
            seen: set.insert(seen, todo_.xy),
          )
        False -> {
          let added_todos =
            grid.neighbours(input, todo_.xy, grid.orthogonal_dirs)
            |> list.filter(fn(neighbour) { neighbour.1 == Ok(todo_.height + 1) })
            |> list.map(fn(neighbour) {
              Trailhead(xy: neighbour.0, height: todo_.height + 1)
            })
          find_trailheads(
            input,
            todos: list.append(rest, added_todos),
            count: count,
            seen: seen,
          )
        }
      }
    }
  }
}
