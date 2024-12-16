import extra
import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import grid.{type XY, Dims}

pub type Robot {
  Robot(p: XY, v: XY)
}

pub fn parse(input: String) -> List(Robot) {
  input
  |> string.split("\n")
  |> list.map(parse_robot)
}

fn parse_robot(line: String) -> Robot {
  case string.split(line, " ") {
    [p, v] -> {
      case p, v {
        "p=" <> pp, "v=" <> vv -> {
          case string.split(pp, ","), string.split(vv, ",") {
            [px, py], [vx, vy] ->
              Robot(p: #(extra.yolo_int(px), extra.yolo_int(py)), v: #(
                extra.yolo_int(vx),
                extra.yolo_int(vy),
              ))
            _, _ -> panic as "Bad input? #3"
          }
        }
        _, _ -> panic as "Bad input? #2"
      }
    }
    _ -> panic as "Bad input? #1"
  }
}

pub fn pt_1(robots: List(Robot)) {
  robots
  |> step_times(100)
  |> io.debug
  |> to_quadrants
  |> io.debug
  |> list.map(list.length)
  |> int.product
}

fn step_times(robots: List(Robot), times: Int) -> List(Robot) {
  use <- bool.guard(when: times <= 0, return: robots)
  step_times(step(robots), times - 1)
}

fn step(robots: List(Robot)) -> List(Robot) {
  list.map(robots, step_robot)
}

pub const room: XY = #(101, 103)

pub const q_size: XY = #(50, 51)

pub const q1: XY = #(0, 0)

pub const q2: XY = #(51, 0)

pub const q3: XY = #(0, 52)

pub const q4: XY = #(51, 52)

pub const q_mid: XY = #(25, 26)

//pub const room: XY = #(11, 7)
//pub const q_size: XY = #(5, 3)
//pub const q1: XY = #(0, 0)
//pub const q2: XY = #(6, 0)
//pub const q3: XY = #(0, 4)
//pub const q4: XY = #(6, 4)

fn step_robot(robot: Robot) -> Robot {
  let new_p: XY =
    robot.p
    |> grid.xy_add(robot.v)
    |> grid.xy_mod(room)
    |> grid.xy_add(room)
    |> grid.xy_mod(room)
  Robot(..robot, p: new_p)
}

fn to_quadrants(robots: List(Robot)) -> List(List(Robot)) {
  [q1, q2, q3, q4]
  |> list.map(robots_in_quadrant(robots, _))
}

fn robots_in_quadrant(robots: List(Robot), q: XY) -> List(Robot) {
  robots
  |> list.filter(fn(robot) {
    robot.p.0 >= q.0
    && robot.p.0 < { q.0 + q_size.0 }
    && robot.p.1 >= q.1
    && robot.p.1 < { q.1 + q_size.1 }
  })
}

pub fn pt_2(robots: List(Robot)) {
  step_pt2(0, robots)
}

fn step_pt2(seconds: Int, robots: List(Robot)) {
  let is_interesting = seconds % 101 == 9
  case is_interesting {
    True -> {
      io.println_error("------------------------")
      io.debug(#(seconds, "seconds"))
      step_pt2(seconds + 1, step(robots))
    }
    False -> step_pt2(seconds + 1, step(robots))
  }
}
