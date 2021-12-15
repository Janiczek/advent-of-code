import gleam/io
import gleam/list
import gleam/set
import gleam/set.{Set}
import gleam/string

type Lights =
  Set(tuple(Int, Int))

type Command {
  Rect(width: Int, height: Int)
  RotateRow(row: Int, amount: Int)
  RotateCol(col: Int, amount: Int)
}

const width = 50
const height = 6

pub fn run() {
  let lights = set.new()

  let cmds = [
    Rect(1, 1),
    RotateRow(0, 5),
    Rect(1, 1),
    RotateRow(0, 6),
    Rect(1, 1),
    RotateRow(0, 5),
    Rect(1, 1),
    RotateRow(0, 2),
    Rect(1, 1),
    RotateRow(0, 5),
    Rect(2, 1),
    RotateRow(0, 2),
    Rect(1, 1),
    RotateRow(0, 4),
    Rect(1, 1),
    RotateRow(0, 3),
    Rect(2, 1),
    RotateRow(0, 7),
    Rect(3, 1),
    RotateRow(0, 3),
    Rect(1, 1),
    RotateRow(0, 3),
    Rect(1, 2),
    RotateRow(1, 13),
    RotateCol(0, 1),
    Rect(2, 1),
    RotateRow(0, 5),
    RotateCol(0, 1),
    Rect(3, 1),
    RotateRow(0, 18),
    RotateCol(13, 1),
    RotateCol(7, 2),
    RotateCol(2, 3),
    RotateCol(0, 1),
    Rect(17, 1),
    RotateRow(3, 13),
    RotateRow(1, 37),
    RotateRow(0, 11),
    RotateCol(7, 1),
    RotateCol(6, 1),
    RotateCol(4, 1),
    RotateCol(0, 1),
    Rect(10, 1),
    RotateRow(2, 37),
    RotateCol(19, 2),
    RotateCol(9, 2),
    RotateRow(3, 5),
    RotateRow(2, 1),
    RotateRow(1, 4),
    RotateRow(0, 4),
    Rect(1, 4),
    RotateCol(25, 3),
    RotateRow(3, 5),
    RotateRow(2, 2),
    RotateRow(1, 1),
    RotateRow(0, 1),
    Rect(1, 5),
    RotateRow(2, 10),
    RotateCol(39, 1),
    RotateCol(35, 1),
    RotateCol(29, 1),
    RotateCol(19, 1),
    RotateCol(7, 2),
    RotateRow(4, 22),
    RotateRow(3, 5),
    RotateRow(1, 21),
    RotateRow(0, 10),
    RotateCol(2, 2),
    RotateCol(0, 2),
    Rect(4, 2),
    RotateCol(46, 2),
    RotateCol(44, 2),
    RotateCol(42, 1),
    RotateCol(41, 1),
    RotateCol(40, 2),
    RotateCol(38, 2),
    RotateCol(37, 3),
    RotateCol(35, 1),
    RotateCol(33, 2),
    RotateCol(32, 1),
    RotateCol(31, 2),
    RotateCol(30, 1),
    RotateCol(28, 1),
    RotateCol(27, 3),
    RotateCol(26, 1),
    RotateCol(23, 2),
    RotateCol(22, 1),
    RotateCol(21, 1),
    RotateCol(20, 1),
    RotateCol(19, 1),
    RotateCol(18, 2),
    RotateCol(16, 2),
    RotateCol(15, 1),
    RotateCol(13, 1),
    RotateCol(12, 1),
    RotateCol(11, 1),
    RotateCol(10, 1),
    RotateCol(7, 1),
    RotateCol(6, 1),
    RotateCol(5, 1),
    RotateCol(3, 2),
    RotateCol(2, 1),
    RotateCol(1, 1),
    RotateCol(0, 1),
    Rect(49, 1),
    RotateRow(2, 34),
    RotateCol(44, 1),
    RotateCol(40, 2),
    RotateCol(39, 1),
    RotateCol(35, 4),
    RotateCol(34, 1),
    RotateCol(30, 4),
    RotateCol(29, 1),
    RotateCol(24, 1),
    RotateCol(15, 4),
    RotateCol(14, 1),
    RotateCol(13, 3),
    RotateCol(10, 4),
    RotateCol(9, 1),
    RotateCol(5, 4),
    RotateCol(4, 3),
    RotateRow(5, 20),
    RotateRow(4, 20),
    RotateRow(3, 48),
    RotateRow(2, 20),
    RotateRow(1, 41),
    RotateCol(47, 5),
    RotateCol(46, 5),
    RotateCol(45, 4),
    RotateCol(43, 5),
    RotateCol(41, 5),
    RotateCol(33, 1),
    RotateCol(32, 3),
    RotateCol(23, 5),
    RotateCol(22, 1),
    RotateCol(21, 2),
    RotateCol(18, 2),
    RotateCol(17, 3),
    RotateCol(16, 2),
    RotateCol(13, 5),
    RotateCol(12, 5),
    RotateCol(11, 5),
    RotateCol(3, 5),
    RotateCol(2, 5),
    RotateCol(1, 5),
  ]

  cmds
  |> list.fold(lights, process)
  |> show()
  |> io.debug()
}

fn show(lights: Lights) -> String {

  let result =
    list.range(0, width)
      |> list.map(fn(col) {
        list.range(0, height)
        |> list.map(fn(row) { 
          case set.contains(lights, tuple(row, col)) {
            True -> "#"
            False -> " "
          }
          })
        |> string.join("")
      })
      |> string.join("\n")

  string.append("\n", result)
}

fn process(command: Command, lights: Lights) -> Lights {
  case command {
    Rect(width, height) -> rect(lights, width, height)
    RotateRow(row, amount) -> rotate_row(lights, row, amount)
    RotateCol(col, amount) -> rotate_col(lights, col, amount)
  }
}

// Rect(AxB (turn on, from top left)
fn rect(lights: Lights, width: Int, height: Int) -> Lights {
  list.range(0, width)
  |> list.map(fn(col) {
    list.range(0, height)
    |> list.map(fn(row) { tuple(row, col) })
  })
  |> list.flatten()
  |> list.fold(lights, fn(coord, acc) { set.insert(acc, coord) })
}

// RotateRow(A,B (to right)
fn rotate_row(lights: Lights, row: Int, amount: Int) -> Lights {
  let row_list =
    list.range(0, width)
    |> list.map(fn(col) { set.contains(lights, tuple(row, col)) })

  let rotated =
    list.append(
      list.drop(row_list, width - amount),
      list.take(row_list, width - amount)
    )
  let rotated_indexed =
    rotated
    |> list.index_map(fn(col, is_on) { tuple(col, is_on) })

  rotated_indexed
  |> list.fold(
    lights,
    fn(item, acc) {
      let tuple(col, is_on) = item
      case is_on {
        False -> set.delete(acc, tuple(row, col))
        True -> set.insert(acc, tuple(row, col))
      }
    },
  )
}

// RotateCol(A,B (to bottom)
fn rotate_col(lights: Lights, col: Int, amount: Int) -> Lights {
  let col_list =
    list.range(0, height)
    |> list.map(fn(row) { set.contains(lights, tuple(row, col)) })

  let rotated =
    list.append(
      list.drop(col_list, height - amount),
      list.take(col_list, height - amount)
    )
  let rotated_indexed =
    rotated
    |> list.index_map(fn(row, is_on) { tuple(row, is_on) })

  rotated_indexed
  |> list.fold(
    lights,
    fn(item, acc) {
      let tuple(row, is_on) = item
      case is_on {
        False -> set.delete(acc, tuple(row, col))
        True -> set.insert(acc, tuple(row, col))
      }
    },
  )
}
