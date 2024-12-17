import extra
import gleam/bool
import gleam/int
import gleam/list
import gleam/string
import grid.{type XY}
import pocket_watch

pub type Machine {
  Machine(a: XY, b: XY, prize: XY)
}

pub fn parse(input: String) -> List(Machine) {
  use <- pocket_watch.simple("parse")
  input
  |> string.split("\n\n")
  |> list.map(parse_machine)
}

fn parse_machine(input: String) -> Machine {
  case string.split(input, "\n") {
    ["Button A: X+" <> but_a, "Button B: X+" <> but_b, "Prize: X=" <> prize] -> {
      case
        string.split(but_a, ", Y+"),
        string.split(but_b, ", Y+"),
        string.split(prize, ", Y=")
      {
        [ax, ay], [bx, by], [px, py] ->
          Machine(
            a: #(extra.yolo_int(ax), extra.yolo_int(ay)),
            b: #(extra.yolo_int(bx), extra.yolo_int(by)),
            prize: #(extra.yolo_int(px), extra.yolo_int(py)),
          )
        _, _, _ -> panic as "Bad input 2?"
      }
    }
    _ -> panic as "Bad input?"
  }
}

pub fn pt_1(input: List(Machine)) {
  use <- pocket_watch.simple("part 1")
  input
  |> list.map(solve_machine)
  |> int.sum
}

fn solve_machine(machine: Machine) -> Int {
  // button A = X+i, Y+j
  // button B = X+l, Y+m
  // prize = (K,N)
  // ---------------------
  // ia + jb = k
  // la + mb = n
  // ---------------------
  // b = (kl-in)/(jl-im)
  // a = (n-mb)/l
  // ---------------------
  // cost = 3 * a + b
  // ---------------------
  // we need to check for integer divisibility and short-circuit to 0
  let #(i, l) = machine.a
  let #(j, m) = machine.b
  let #(k, n) = machine.prize
  use <- bool.guard(when: { k * l - i * n } % { j * l - i * m } != 0, return: 0)
  let b = { k * l - i * n } / { j * l - i * m }
  use <- bool.guard(when: { n - m * b } % l != 0, return: 0)
  let a = { n - m * b } / l
  3 * a + b
}

pub fn pt_2(input: List(Machine)) {
  use <- pocket_watch.simple("part 2")
  input
  |> list.map(fn(machine) {
    Machine(
      ..machine,
      prize: grid.xy_add(machine.prize, #(
        10_000_000_000_000,
        10_000_000_000_000,
      )),
    )
  })
  |> list.map(solve_machine)
  |> int.sum
}
