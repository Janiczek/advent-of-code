import gleam/int
import gleam/list
import gleam/result
import gleam/string
import grid.{type Grid, type XY}

pub type XGrid =
  Grid(String)

pub fn parse(input: String) -> XGrid {
  grid.from_string(input)
}

pub fn pt_1(grid: XGrid) {
  grid
  |> grid.map(fn(xy, _) { xmas_count(grid, xy) })
  |> grid.values
  |> int.sum
}

pub fn pt_2(grid: XGrid) {
  grid
  |> grid.map(fn(xy, _) { x_mas_count(grid, xy) })
  |> grid.values
  |> int.sum
}

fn xmas_count(grid_: XGrid, xy: XY) -> Int {
  grid.all_dirs
  |> list.count(fn(dir) {
    grid.in_dir(grid_, xy, dir, 4)
    |> result.map(fn(xys) {
      grid.get_all(grid_, xys)
      |> result.map(fn(all) { string.join(all, "") == "XMAS" })
      |> result.unwrap(False)
    })
    |> result.unwrap(False)
  })
}

fn x_mas_count(grid_: XGrid, xy: XY) -> Int {
  case grid.get(grid_, xy) {
    Ok("A") -> {
      let x_deltas = list.map(grid.diagonal_dirs, grid.delta)
      let xys = x_deltas |> list.map(fn(d) { grid.xy_add(xy, d) })
      case grid.get_all(grid_, xys) {
        Error(Nil) -> 0
        // TR BR BL TL
        // Theoretically we could do "if the list is a palindrome"?
        Ok(["M", "M", "S", "S"])
        | Ok(["M", "S", "S", "M"])
        | Ok(["S", "M", "M", "S"])
        | Ok(["S", "S", "M", "M"]) -> 1
        Ok(_) -> 0
      }
    }
    _ -> 0
  }
}
