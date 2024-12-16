import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import grid.{type Dir, type Grid, type XY}

pub type Input {
  Input(grid: Grid(Entity), robot: XY, moves: List(Dir))
}

pub type Entity {
  Wall
  Box
}

pub fn parse(input: String) -> Input {
  case string.split(input, "\n\n") {
    [map, moves] -> {
      let char_grid = grid.from_string(map)
      Input(
        grid: char_grid
          |> grid.filter_map(fn(_, c) {
            case c {
              "#" -> Ok(Wall)
              "O" -> Ok(Box)
              _ -> Error(Nil)
            }
          }),
        robot: case grid.find_exact(char_grid, "@") {
          Ok(xy) -> xy
          Error(Nil) -> panic as "Bad input? #3"
        },
        moves: moves
          |> string.replace(each: "\n", with: "")
          |> string.to_graphemes
          |> list.map(fn(c) {
            case c {
              "<" -> grid.Left
              ">" -> grid.Right
              "^" -> grid.Top
              "v" -> grid.Bottom
              _ -> panic as "Bad input? #2"
            }
          }),
      )
    }
    _ -> panic as "Bad input? #1"
  }
}

pub fn pt_1(input: Input) {
  input.moves
  |> list.fold(from: #(input.grid, input.robot), with: step)
  |> fn(state) { state.0 }
  |> grid.filter(fn(_, e) { e == Box })
  |> grid.keys
  |> list.map(gps)
  |> int.sum
}

pub fn pt_2(input: Input) {
  let wider_grid = widen(input.grid)
  let wider_robot = grid.xy_mul(input.robot, #(2, 1))
  let #(final_grid, _) =
    input.moves
    |> list.fold(from: #(wider_grid, wider_robot), with: wider_step)
  final_grid
  |> grid.filter(fn(_, e) { e == BoxLeft })
  |> grid.keys
  |> list.map(gps)
  |> int.sum
}

fn gps(xy: XY) -> Int {
  100 * xy.1 + xy.0
}

// Pt 1:
// when next to robot:
//   nothing -> just move the robot
//   wall -> do nothing
//   box -> find the next item
//      nothing -> move the first box to the free space,
//                 move the robot to the box position
//      wall -> do nothing
//      box -> recurse (but remember the first box XY)

fn step(state: #(Grid(Entity), XY), dir: Dir) -> #(Grid(Entity), XY) {
  let #(grid, robot) = state
  let next_xy = grid.step(robot, dir, 1)
  case grid.get(grid, next_xy) {
    Error(Nil) -> #(grid, next_xy)
    Ok(Wall) -> state
    Ok(Box) -> step_find_nonbox(grid, robot, next_xy, next_xy, dir)
  }
}

fn step_find_nonbox(
  grid: Grid(Entity),
  robot: XY,
  first_box_xy: XY,
  current_xy: XY,
  dir: Dir,
) -> #(Grid(Entity), XY) {
  let next_xy = grid.step(current_xy, dir, 1)
  case grid.get(grid, next_xy) {
    Error(Nil) -> #(
      grid.move(grid, from: first_box_xy, to: next_xy),
      first_box_xy,
    )
    Ok(Wall) -> #(grid, robot)
    Ok(Box) -> step_find_nonbox(grid, robot, first_box_xy, next_xy, dir)
  }
}

pub type Entity2 {
  Wall2
  BoxLeft
  BoxRight
}

fn widen(grid: Grid(Entity)) -> Grid(Entity2) {
  grid
  |> grid.to_list
  |> list.flat_map(fn(kv) {
    let #(xy, e) = kv
    let xy1 = grid.xy_mul(xy, #(2, 1))
    let xy2 = grid.xy_add(xy1, #(1, 0))
    let #(first, second) = case e {
      Wall -> #(Wall2, Wall2)
      Box -> #(BoxLeft, BoxRight)
    }
    [#(xy1, first), #(xy2, second)]
  })
  |> grid.from_list
}

fn wider_step(state: #(Grid(Entity2), XY), dir: Dir) -> #(Grid(Entity2), XY) {
  let #(grid, robot) = state
  let next_xy = grid.step(robot, dir, 1)
  case grid.get(grid, next_xy) {
    Error(Nil) -> #(grid, next_xy)
    Ok(Wall2) -> state
    Ok(e) -> {
      let left = left_in_dir(e, next_xy, dir)
      wider_step_find_nonbox(grid, robot, [left], [left], dir)
    }
  }
}

fn right_for_left(left: XY) {
  grid.step(left, grid.Right, 1)
}

fn left_for_right(right: XY) {
  grid.step(right, grid.Left, 1)
}

fn wider_step_find_nonbox(
  grid: Grid(Entity2),
  robot: XY,
  moved_lefts: List(XY),
  frontier_lefts: List(XY),
  dir: Dir,
) -> #(Grid(Entity2), XY) {
  case list.is_empty(frontier_lefts) {
    True -> {
      let box_parts =
        moved_lefts
        |> list.flat_map(fn(left) {
          [#(left, BoxLeft), #(right_for_left(left), BoxRight)]
        })
      // can move
      #(
        grid
          |> grid.delete_all(box_parts |> list.map(fn(kv) { kv.0 }))
          |> grid.insert_all(
            box_parts
            |> list.map(fn(kv) { #(grid.step(kv.0, dir, 1), kv.1) }),
          ),
        grid.step(robot, dir, 1),
      )
    }
    False -> {
      let frontier_lefts_next: List(#(XY, Entity2)) =
        frontier_lefts
        |> list.flat_map(fn(left) {
          case grid.get(grid, left) {
            Ok(Wall2) -> panic as "wall as a frontier?!"
            Ok(BoxLeft) -> {
              let right = right_for_left(left)
              let frontmost: List(XY) = case dir {
                grid.Left -> [left]
                grid.Right -> [right]
                grid.Top -> [left, right]
                grid.Bottom -> [left, right]
                _ -> panic as "Diagonals?? In my program??"
              }
              let frontmost_touches: List(XY) =
                frontmost
                |> list.map(grid.step(_, dir, 1))
              case
                dir,
                frontmost_touches,
                list.map(frontmost_touches, grid.get(grid, _))
              {
                // Left
                grid.Left, [l], [Ok(Wall2)] -> [#(l, Wall2)]
                grid.Left, [_], [Ok(BoxLeft)] -> panic as "split box #1"
                grid.Left, [l], [Ok(BoxRight)] -> [
                  #(left_for_right(l), BoxLeft),
                ]
                grid.Left, [_], [Error(Nil)] -> []

                // Right
                grid.Right, [r], [Ok(Wall2)] -> [#(r, Wall2)]
                grid.Right, [r], [Ok(BoxLeft)] -> [#(r, BoxLeft)]
                grid.Right, [_], [Ok(BoxRight)] -> panic as "split box #2"
                grid.Right, [_], [Error(Nil)] -> []

                // Top
                grid.Top, [l, r], [Ok(Wall2), Ok(Wall2)] -> [
                  #(l, Wall2),
                  #(r, Wall2),
                ]
                grid.Top, [l, _], [Ok(Wall2), _] -> [#(l, Wall2)]
                grid.Top, [_, r], [_, Ok(Wall2)] -> [#(r, Wall2)]
                grid.Top, [l, r], [Ok(BoxRight), Ok(BoxLeft)] -> [
                  #(left_for_right(l), BoxLeft),
                  #(r, BoxLeft),
                ]
                grid.Top, [l, _], [Ok(BoxLeft), Ok(BoxRight)] -> [#(l, BoxLeft)]
                grid.Top, [_, _], [Error(Nil), Error(Nil)] -> []
                grid.Top, [_, r], [Error(Nil), Ok(BoxLeft)] -> [#(r, BoxLeft)]
                grid.Top, [l, _], [Ok(BoxRight), Error(Nil)] -> [
                  #(left_for_right(l), BoxLeft),
                ]

                // Bottom
                grid.Bottom, [l, r], [Ok(Wall2), Ok(Wall2)] -> [
                  #(l, Wall2),
                  #(r, Wall2),
                ]
                grid.Bottom, [l, _], [Ok(Wall2), _] -> [#(l, Wall2)]
                grid.Bottom, [_, r], [_, Ok(Wall2)] -> [#(r, Wall2)]
                grid.Bottom, [l, r], [Ok(BoxRight), Ok(BoxLeft)] -> [
                  #(left_for_right(l), BoxLeft),
                  #(r, BoxLeft),
                ]
                grid.Bottom, [l, _], [Ok(BoxLeft), Ok(BoxRight)] -> [
                  #(l, BoxLeft),
                ]
                grid.Bottom, [_, _], [Error(Nil), Error(Nil)] -> []
                grid.Bottom, [_, r], [Error(Nil), Ok(BoxLeft)] -> [
                  #(r, BoxLeft),
                ]
                grid.Bottom, [l, _], [Ok(BoxRight), Error(Nil)] -> [
                  #(left_for_right(l), BoxLeft),
                ]

                // Unfinished cases
                _, _, _ -> {
                  io.debug(#(
                    dir,
                    frontmost_touches,
                    list.map(frontmost_touches, grid.get(grid, _)),
                  ))
                  todo
                }
              }
            }
            Ok(BoxRight) -> panic as "right as a frontier?!"
            Error(Nil) -> []
          }
        })
      use <- bool.guard(
        when: list.any(frontier_lefts_next, fn(kv) { kv.1 == Wall2 }),
        return: #(grid, robot),
      )
      let frontier_lefts_next_xys =
        frontier_lefts_next |> list.map(fn(kv) { kv.0 })
      wider_step_find_nonbox(
        grid,
        robot,
        list.append(frontier_lefts_next_xys, moved_lefts),
        frontier_lefts_next_xys,
        dir,
      )
    }
  }
}

fn left_in_dir(e: Entity2, xy: XY, dir: Dir) -> XY {
  case dir, e {
    _, Wall2 -> panic as "We shouldn't call this with Wall2"
    grid.Left, BoxLeft -> panic as "huh? #1"
    grid.Left, BoxRight -> left_for_right(xy)
    grid.Right, BoxLeft -> xy
    grid.Right, BoxRight -> panic as "huh? #2"
    grid.Top, BoxLeft -> xy
    grid.Top, BoxRight -> left_for_right(xy)
    grid.Bottom, BoxLeft -> xy
    grid.Bottom, BoxRight -> left_for_right(xy)
    _, _ -> panic as "diagonals? huh?"
  }
}
