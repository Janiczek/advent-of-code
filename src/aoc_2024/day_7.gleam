import extra
import gleam/int
import gleam/list
import gleam/string

pub type Row {
  Row(result: Int, operands: List(Int))
}

pub fn parse(input: String) -> List(Row) {
  string.split(input, "\n")
  |> list.map(fn(line) {
    case string.split(line, ": ") {
      [result_str, operands_str] -> {
        Row(
          result: extra.yolo_int(result_str),
          operands: string.split(operands_str, " ")
            |> list.map(extra.yolo_int),
        )
      }
      _ -> panic as "Bad input 1"
    }
  })
}

pub fn pt_1(input: List(Row)) {
  input
  |> list.filter(fn(row) { can_be_combined(row, [int.add, int.multiply]) })
  |> list.map(fn(row) { row.result })
  |> int.sum
}

pub fn pt_2(input: List(Row)) {
  input
  |> list.filter(fn(row) {
    can_be_combined(row, [int.add, int.multiply, concat_digits])
  })
  |> list.map(fn(row) { row.result })
  |> int.sum
}

fn can_be_combined(row: Row, ops: List(fn(Int, Int) -> Int)) -> Bool {
  case row.operands {
    [first, ..rest] -> search(row.result, [#(first, rest)], ops)
    _ -> panic as "No operands?!"
  }
}

fn concat_digits(a: Int, b: Int) -> Int {
  // TODO could be faster if we do math (a * log10(b) + b)
  extra.yolo_int(int.to_string(a) <> int.to_string(b))
}

fn search(
  result: Int,
  todos: List(#(Int, List(Int))),
  ops: List(fn(Int, Int) -> Int),
) -> Bool {
  case todos {
    [] -> False
    [#(acc_sum, operands), ..rest_of_todos] -> {
      case operands {
        [] ->
          case acc_sum == result {
            True -> True
            False -> search(result, rest_of_todos, ops)
          }
        [operand, ..rest_of_operands] -> {
          case acc_sum > result {
            True -> search(result, rest_of_todos, ops)
            False ->
              search(
                result,
                list.append(
                  list.map(ops, fn(op) {
                    #(op(acc_sum, operand), rest_of_operands)
                  }),
                  rest_of_todos,
                ),
                ops,
              )
          }
        }
      }
    }
  }
}
