import extra
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/yielder.{type Yielder}
import grid.{type Dir, type Grid, type XY}
import pocket_watch

pub type Input {
  Input(grid: Grid(Nil), start: XY, end: XY)
}

pub fn parse(input: String) -> Input {
  use <- pocket_watch.simple("parse")
  let input_grid = grid.from_string(input)
  let assert Ok(start) = grid.find_exact(input_grid, "S")
  let assert Ok(end) = grid.find_exact(input_grid, "E")
  let grid =
    input_grid
    |> grid.filter_map(fn(_, c) {
      case c {
        "#" -> Error(Nil)
        "." -> Ok(Nil)
        "S" -> Ok(Nil)
        "E" -> Ok(Nil)
        _ -> panic as "Bad input?"
      }
    })
  Input(grid, start, end)
}

pub fn pt_1(input: Input) {
  use <- pocket_watch.simple("part 1")
  find(input, 2, 100)
}

pub fn pt_2(input: Input) {
  use <- pocket_watch.simple("part 2")
  find(input, 20, 100)
}

pub fn find(input: Input, teleport: Int, threshold: Int) {
  let track: List(XY) = grid.find_all_exact(input.grid, Nil)
  let assert Ok(#(_, astar_bests)) =
    grid.astar_with_bests(
      from: input.start,
      to: input.end,
      cost: grid.manhattan_distance,
      neighbours: fn(xy) {
        grid.neighbours(input.grid, xy, grid.orthogonal_dirs)
        |> list.filter(fn(kv) { result.is_ok(kv.1) })
        |> list.map(fn(kv) { kv.0 })
      },
      print_progress: fn(_, _, _) { Nil },
    )

  let possibilities: Yielder(#(XY, XY)) =
    track
    |> yielder.from_list
    |> extra.yield_combination_pairs
    |> yielder.filter(fn(pair) {
      grid.manhattan_distance(pair.0, pair.1) <= teleport
    })

  possibilities
  |> yielder.filter(fn(tp) {
    let assert Ok(old_cost) = dict.get(astar_bests, tp.1)
    let assert Ok(start_cost) = dict.get(astar_bests, tp.0)
    let savings =
      int.absolute_value(old_cost - start_cost)
      - grid.manhattan_distance(tp.0, tp.1)
    savings >= threshold
  })
  |> yielder.length
}
