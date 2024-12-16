import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleam/result
import gleam/set.{type Set}
import gleamy/priority_queue.{type Queue}
import grid.{type Dir, type Grid, type XY}
import pocket_watch

pub type Input {
  Input(grid: Grid(Nil), reindeer: XY, exit: XY)
}

pub fn parse(input: String) -> Input {
  use <- pocket_watch.simple("parse")
  let grid: Grid(Nil) =
    input
    |> grid.from_string
    |> grid.filter_map(fn(_, c) {
      case c {
        "#" -> Ok(Nil)
        _ -> Error(Nil)
      }
    })

  Input(
    grid: grid,
    reindeer: #(grid.dims.min_x + 1, grid.dims.max_y - 1),
    exit: #(grid.dims.max_x - 1, grid.dims.min_y + 1),
  )
}

pub fn pt_1(input: Input) {
  use <- pocket_watch.simple("part 1")
  pt_1_aux(
    input,
    todos: priority_queue.from_list(
      [
        Todo(
          current_xy: input.reindeer,
          current_dir: grid.Right,
          positions: set.new(),
          cost: 0,
        ),
      ],
      compare_todo,
    ),
    seen: dict.new(),
  )
}

type Step {
  RotateCW
  RotateCCW
  Forward
}

type Todo {
  Todo(current_xy: XY, current_dir: Dir, positions: Set(#(XY, Dir)), cost: Int)
}

fn compare_todo(a: Todo, b: Todo) {
  int.compare(a.cost, b.cost)
}

fn pt_1_neighbours(grid: Grid(Nil), xy: XY, dir: Dir) -> List(Step) {
  case grid.get(grid, grid.step(xy, dir, 1)) {
    Ok(Nil) -> [RotateCW, RotateCCW]
    Error(Nil) -> [RotateCW, RotateCCW, Forward]
  }
}

fn apply_step(step: Step, xy: XY, dir: Dir) -> #(XY, Dir) {
  case step {
    RotateCW -> #(xy, grid.rotate_cw(dir))
    RotateCCW -> #(xy, grid.rotate_ccw(dir))
    Forward -> #(grid.step(xy, dir, 1), dir)
  }
}

fn pt_1_aux(
  input: Input,
  todos todos: Queue(Todo),
  seen seen: Dict(#(XY, Dir), Int),
) -> Result(Int, Nil) {
  use #(todo_, rest_of_todos) <- result.try(priority_queue.pop(todos))
  use <- bool.guard(
    when: todo_.current_xy == input.exit,
    return: Ok(todo_.cost),
  )
  let new_seen =
    dict.insert(seen, #(todo_.current_xy, todo_.current_dir), todo_.cost)
  let new_neighbours: List(Step) =
    pt_1_neighbours(input.grid, todo_.current_xy, todo_.current_dir)
    |> list.filter(fn(neighbour) {
      let neighbour_applied =
        apply_step(neighbour, todo_.current_xy, todo_.current_dir)
      case dict.get(seen, neighbour_applied) {
        Error(Nil) -> True
        Ok(previously_best_cost) ->
          previously_best_cost > todo_.cost + find_cost(neighbour)
      }
    })
  let added_todos =
    new_neighbours
    |> list.map(fn(neighbour) {
      let neighbour_applied =
        apply_step(neighbour, todo_.current_xy, todo_.current_dir)
      Todo(
        current_xy: neighbour_applied.0,
        current_dir: neighbour_applied.1,
        positions: set.insert(todo_.positions, neighbour_applied),
        cost: todo_.cost + find_cost(neighbour),
      )
    })
  let new_todos =
    list.fold(over: added_todos, from: rest_of_todos, with: priority_queue.push)
  pt_1_aux(input, todos: new_todos, seen: new_seen)
}

fn find_cost(step: Step) -> Int {
  case step {
    RotateCCW -> 1000
    RotateCW -> 1000
    Forward -> 1
  }
}

pub fn pt_2(input: Input) {
  use <- pocket_watch.simple("part 2")
  let steps =
    pt_2_aux(
      input,
      todos: priority_queue.from_list(
        [
          Todo(
            current_xy: input.reindeer,
            current_dir: grid.Right,
            positions: set.new(),
            cost: 0,
          ),
        ],
        compare_todo,
      ),
      seen: dict.new(),
      best: 109_496,
    )
  //io.println_error(
  //  grid.to_string_simple(input.grid, fn(xy, c) {
  //    case set.contains(steps, xy), c {
  //      True, Error(Nil) -> "O"
  //      True, Ok(Nil) -> panic as "walked over a wall?"
  //      False, Error(Nil) -> "."
  //      False, Ok(Nil) -> "#"
  //    }
  //  }),
  //)

  set.size(steps)
}

fn pt_2_final(exit: XY, seen: Dict(#(XY, Dir), #(Int, Set(XY)))) -> Set(XY) {
  case
    seen
    |> dict.filter(fn(k, _) { k.0 == exit })
    |> dict.values
    |> list.group(by: fn(kv) { kv.0 })
    |> dict.to_list
    |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
    |> list.first
  {
    Ok(best) ->
      best.1
      |> list.flat_map(fn(x) {
        x.1
        |> set.to_list
      })
      |> set.from_list
    Error(Nil) -> panic as "no solution?"
  }
}

fn pt_2_aux(
  input: Input,
  todos todos: Queue(Todo),
  seen seen: Dict(#(XY, Dir), #(Int, Set(XY))),
  best best: Int,
) -> Set(XY) {
  case priority_queue.pop(todos) {
    Error(Nil) -> pt_2_final(input.exit, seen)
    Ok(#(todo_, rest_of_todos)) -> {
      case todo_.cost > best {
        True -> pt_2_aux(input, todos: rest_of_todos, seen: seen, best: best)
        False -> {
          case todo_.current_xy == input.exit {
            True -> {
              let new_seen =
                dict.upsert(
                  seen,
                  #(todo_.current_xy, todo_.current_dir),
                  with: fn(existing) {
                    case existing {
                      None -> #(
                        todo_.cost,
                        set.map(todo_.positions, fn(x) { x.0 }),
                      )
                      Some(existing_) ->
                        case int.compare(todo_.cost, existing_.0) {
                          order.Lt -> #(
                            todo_.cost,
                            set.map(todo_.positions, fn(x) { x.0 }),
                          )
                          order.Eq -> #(
                            todo_.cost,
                            set.union(
                              existing_.1,
                              set.map(todo_.positions, fn(x) { x.0 }),
                            ),
                          )
                          order.Gt -> existing_
                        }
                    }
                  },
                )
              //io.debug(#(
              //  "found",
              //  todo_.cost,
              //  set.size(pt_2_final(input.exit, new_seen)),
              //))
              let new_best = int.min(best, todo_.cost)
              pt_2_aux(
                input,
                todos: rest_of_todos,
                seen: new_seen,
                best: new_best,
              )
            }
            False -> {
              case dict.get(seen, #(todo_.current_xy, todo_.current_dir)) {
                Ok(seen_) if seen_.0 < todo_.cost ->
                  pt_2_aux(input, todos: rest_of_todos, seen: seen, best: best)
                _ -> {
                  let new_neighbours: List(Step) =
                    pt_1_neighbours(
                      input.grid,
                      todo_.current_xy,
                      todo_.current_dir,
                    )
                  let added_todos =
                    new_neighbours
                    |> list.map(fn(neighbour) {
                      let neighbour_applied =
                        apply_step(
                          neighbour,
                          todo_.current_xy,
                          todo_.current_dir,
                        )
                      Todo(
                        current_xy: neighbour_applied.0,
                        current_dir: neighbour_applied.1,
                        positions: set.insert(
                          todo_.positions,
                          neighbour_applied,
                        ),
                        cost: todo_.cost + find_cost(neighbour),
                      )
                    })
                  let new_todos =
                    list.fold(
                      over: added_todos,
                      from: rest_of_todos,
                      with: priority_queue.push,
                    )
                  pt_2_aux(
                    input,
                    todos: new_todos,
                    seen: dict.upsert(
                      seen,
                      #(todo_.current_xy, todo_.current_dir),
                      with: fn(existing) {
                        case existing {
                          None -> #(todo_.cost, set.new())
                          Some(existing_) -> #(
                            todo_.cost,
                            set.union(existing_.1, set.new()),
                          )
                        }
                      },
                    ),
                    best: best,
                  )
                }
              }
            }
          }
        }
      }
    }
  }
}
