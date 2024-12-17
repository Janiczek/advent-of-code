import gleam/dict
import gleam/list
import gleam/set
import grid.{type Dims, type Grid, type XY}
import pocket_watch

pub fn parse(input: String) -> Grid(String) {
  use <- pocket_watch.simple("parse")
  grid.from_string(input)
  |> grid.filter(fn(_, c) { c != "." })
}

pub fn pt_1(input: Grid(String)) {
  use <- pocket_watch.simple("part 1")
  input
  |> grid.to_list
  |> list.group(by: fn(cell) { cell.1 })
  |> dict.values
  |> list.map(fn(nodes) { list.map(nodes, fn(node) { node.0 }) })
  |> list.fold(from: set.new(), with: fn(acc, nodes) {
    nodes
    |> list.combination_pairs
    |> list.flat_map(antinodes_xy_pt1)
    |> list.filter(fn(xy) { grid.in_grid(input, xy) })
    |> set.from_list
    |> set.union(acc)
  })
  |> set.size
}

fn antinodes_xy_pt1(pair: #(XY, XY)) -> List(XY) {
  let #(a, b) = pair
  let diff = grid.xy_sub(a, b)
  [grid.xy_add(a, diff), grid.xy_sub(b, diff)]
}

pub fn pt_2(input: Grid(String)) {
  use <- pocket_watch.simple("part 2")
  input
  |> grid.to_list
  |> list.group(by: fn(cell) { cell.1 })
  |> dict.values
  |> list.map(fn(nodes) { list.map(nodes, fn(node) { node.0 }) })
  |> list.fold(from: set.new(), with: fn(acc, nodes) {
    nodes
    |> list.combination_pairs
    |> list.flat_map(antinodes_xy_pt2(input.dims, _))
    |> set.from_list
    |> set.union(acc)
  })
  |> set.size
}

fn antinodes_xy_pt2(dims: Dims, pair: #(XY, XY)) -> List(XY) {
  let #(a, b) = pair
  list.append(
    cast_ray(dims, a, grid.xy_sub(a, b), [a]),
    cast_ray(dims, b, grid.xy_sub(b, a), [b]),
  )
}

fn cast_ray(dims: Dims, start: XY, d: XY, acc: List(XY)) -> List(XY) {
  let next = grid.xy_add(start, d)
  case grid.in_dims(dims, next) {
    False -> acc
    True -> cast_ray(dims, next, d, [next, ..acc])
  }
}
