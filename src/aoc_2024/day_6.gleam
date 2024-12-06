import gleam/set.{type Set}
import grid.{type Dir, type Grid, type XY}

pub type Input {
  Input(grid: Grid(Nil), guard_xy: XY)
}

pub fn parse(input: String) -> Input {
  let g = grid.from_string(input)
  let assert Ok(guard_xy) = grid.find_exact(g, "^")
  Input(
    grid: grid.filter_map(g, fn(_, c) {
      case c {
        "#" -> Ok(Nil)
        _ -> Error(Nil)
      }
    }),
    guard_xy: guard_xy,
  )
}

pub fn pt_1(input: Input) {
  pt_1_step(
    input.grid,
    set.from_list([input.guard_xy]),
    input.guard_xy,
    grid.Top,
  )
  |> set.size()
}

fn pt_1_step(g: Grid(Nil), seen: Set(XY), current_xy: XY, dir: Dir) -> Set(XY) {
  let next_xy = grid.step(current_xy, dir, 1)
  case grid.get(g, next_xy) {
    Ok(_wall) -> pt_1_step(g, seen, current_xy, grid.turn_90deg_right(dir))
    Error(_) ->
      case grid.in_grid(g, next_xy) {
        True -> pt_1_step(g, set.insert(seen, next_xy), next_xy, dir)
        False -> seen
      }
  }
}

pub fn pt_2(input: Input) {
  let orig_path =
    pt_1_step(
      input.grid,
      set.from_list([input.guard_xy]),
      input.guard_xy,
      grid.Top,
    )
  orig_path
  |> set.filter(fn(step: XY) {
    pt_2_has_loop(
      grid.insert(input.grid, step, Nil),
      set.new(),
      input.guard_xy,
      grid.Top,
    )
  })
  |> set.size
}

fn pt_2_has_loop(
  g: Grid(Nil),
  seen: Set(#(XY, Dir)),
  current_xy: XY,
  dir: Dir,
) -> Bool {
  case set.contains(seen, #(current_xy, dir)) {
    True -> True
    False -> {
      let new_seen = set.insert(seen, #(current_xy, dir))
      let next_xy = grid.step(current_xy, dir, 1)
      case grid.get(g, next_xy) {
        Ok(_wall) -> {
          let next_dir = grid.turn_90deg_right(dir)
          pt_2_has_loop(g, new_seen, current_xy, next_dir)
        }
        Error(_) ->
          case grid.in_grid(g, next_xy) {
            True -> pt_2_has_loop(g, new_seen, next_xy, dir)
            False -> False
          }
      }
    }
  }
}
