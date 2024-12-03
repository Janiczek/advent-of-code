import extra
import gleam/int
import gleam/list
import gleam/regexp
import gleam/string

pub type Op {
  Mul(Int, Int)
  Do
  Dont
}

fn from_match(match: regexp.Match) -> Op {
  case match.content {
    "mul(" <> rest -> {
      let assert Ok(strs) =
        rest
        |> string.drop_end(1)
        |> string.split_once(",")
      Mul(extra.yolo_int(strs.0), extra.yolo_int(strs.1))
    }
    "do()" -> Do
    "don't()" -> Dont
    _ -> panic as "Bad input"
  }
}

pub fn parse(input: String) -> List(Op) {
  let assert Ok(regex) =
    regexp.from_string("mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)")

  regex
  |> regexp.scan(input)
  |> list.map(from_match)
}

pub fn pt_1(ops: List(Op)) {
  ops
  |> list.map(fn(op) {
    case op {
      Mul(x, y) -> x * y
      Do -> 0
      Dont -> 0
    }
  })
  |> int.sum
}

type State {
  State(sum: Int, can_mult: Bool)
}

pub fn pt_2(ops: List(Op)) {
  let result =
    ops
    |> list.fold(State(0, True), fn(acc, op) {
      case op {
        Mul(x, y) ->
          case acc.can_mult {
            True -> State(..acc, sum: acc.sum + x * y)
            False -> acc
          }
        Do -> State(..acc, can_mult: True)
        Dont -> State(..acc, can_mult: False)
      }
    })

  result.sum
}
