import extra
import gleam/int
import gleam/list
import gleam/set.{type Set}
import grid.{type Grid, type XY}
import pocket_watch

pub fn parse(input: String) -> Grid(String) {
  use <- pocket_watch.simple("parse")
  grid.from_string(input)
}

pub fn pt_1(input: Grid(String)) {
  use <- pocket_watch.simple("part 1")
  grid.group_consecutive(input, grid.orthogonal_dirs)
  |> list.map(fn(kv) { area(kv.1) * perimeter(kv.1) })
  |> int.sum
}

pub fn pt_2(input: Grid(String)) {
  use <- pocket_watch.simple("part 2")
  //io.println_error(grid.to_string(
  //  input,
  //  fun: fn(s) { #(s, Error(Nil)) },
  //  empty: ".",
  //))

  let groups: List(#(String, List(XY))) =
    grid.group_consecutive(input, grid.orthogonal_dirs)

  //list.each(groups, io.debug)

  let #(distinguished_grid, distinguished_groups): #(
    Grid(Int),
    List(#(Int, List(XY))),
  ) = distinguish_groups(input, groups)

  //io.println_error(grid.to_string(
  //  distinguished_grid,
  //  fun: fn(i) { #(int.to_string(i), Error(Nil)) },
  //  empty: ".",
  //))

  //list.each(distinguished_groups, io.debug)

  let fences: Grid(Fence) =
    distinguished_grid
    |> grid.extend_edge(with: 0)
    |> to_fences

  //io.println_error(grid.to_string(
  //  fences,
  //  fun: fn(fence) {
  //    #(
  //      fence.ids |> list.map(int.to_string) |> string.concat <> " ",
  //      Error(Nil),
  //    )
  //  },
  //  empty: ".",
  //))

  list.map2(groups, distinguished_groups, fn(_, kv) {
    let area_ = area(kv.1)
    let sides_ = sides(fences, group_id: kv.0)
    let result = area_ * sides_
    //io.debug(#(old.0, kv.0, "area", area_, "sides", sides_, "=", result))
    result
  })
  |> int.sum
}

fn area(group: List(XY)) -> Int {
  list.length(group)
}

fn perimeter(group: List(XY)) -> Int {
  edges(group)
  |> filter_external
  |> list.length
}

fn edges(group: List(XY)) -> List(#(XY, XY)) {
  group
  |> list.flat_map(fn(rect) {
    let nw = #(rect.0, rect.1)
    let ne = #(rect.0 + 1, rect.1)
    let sw = #(rect.0, rect.1 + 1)
    let se = #(rect.0 + 1, rect.1 + 1)
    let n = #(nw, ne)
    let e = #(ne, se)
    let s = #(se, sw)
    let w = #(sw, nw)
    [n, e, s, w]
  })
}

fn filter_external(edges: List(#(XY, XY))) -> List(#(XY, XY)) {
  let edges_set: Set(#(XY, XY)) = set.from_list(edges)
  let reverse = fn(edge: #(XY, XY)) { #(edge.1, edge.0) }

  // internal edges are present twice (once reversed)
  // external edges are only there once
  edges
  |> list.filter(fn(edge) { !set.contains(edges_set, reverse(edge)) })
}

fn distinguish_groups(
  input: Grid(String),
  groups: List(#(String, List(XY))),
) -> #(Grid(Int), List(#(Int, List(XY)))) {
  // 1-based (to leave room for "0" edges from grid.extend_edge later)
  // AAA    111
  // ABA -> 121
  // AAA    111
  let #(new_grid, new_groups, _) =
    groups
    |> list.fold(from: #(input, [], 1), with: fn(acc, group) {
      let #(acc_grid, acc_groups, acc_i) = acc
      let i_str = int.to_string(acc_i)
      let new_grid =
        group.1
        |> list.fold(from: acc_grid, with: fn(acc_grid_2, xy) {
          grid.set(acc_grid_2, xy, i_str)
        })
      let new_groups = [#(acc_i, group.1), ..acc_groups]
      #(new_grid, new_groups, acc_i + 1)
    })

  #(
    grid.map(new_grid, fn(_, id) { extra.yolo_int(id) }),
    list.reverse(new_groups),
  )
}

type Fence {
  // uses the [nw, ne, sw, se] order
  Fence(ids: List(Int))
}

fn to_fences(input: Grid(Int)) -> Grid(Fence) {
  // TODO is this some kind of grid.stencil generic helper?

  // Creates a grid that is one less in dimensions than the input (because
  // instead of being inside the cells it's in the "lines between them")

  // Looks at the diagonal neighbours of each cell and records them to a Fence
  input
  |> grid.map(fn(xy, id) {
    let ids: List(Int) = [
      id,
      ..{
        grid.neighbours(input, xy, [grid.Right, grid.Bottom, grid.BottomRight])
        |> list.filter_map(fn(kv) { kv.1 })
      }
    ]
    Fence(ids)
  })
  |> grid.remove_bottom_row
  |> grid.remove_right_column
}

type Mask {
  // not equal to the group_id
  Ne

  // equal to the group_id
  Eq
}

fn sides(fences: Grid(Fence), group_id group_id: Int) -> Int {
  let tlo = [Ne, Ne, Ne, Eq]
  let tro = [Ne, Ne, Eq, Ne]
  let blo = [Ne, Eq, Ne, Ne]
  let bro = [Eq, Ne, Ne, Ne]

  let tri = [Eq, Ne, Eq, Eq]
  let tli = [Ne, Eq, Eq, Eq]
  let bli = [Eq, Eq, Eq, Ne]
  let bri = [Eq, Eq, Ne, Eq]

  let checkerboard_tl_br = [Eq, Ne, Ne, Eq]
  let checkerboard_tr_bl = [Ne, Eq, Eq, Ne]
  // the items above are in the [nw, ne, sw, se] order
  let count_tlo = count(fences, group_id, tlo)
  let count_tro = count(fences, group_id, tro)
  let count_blo = count(fences, group_id, blo)
  let count_bro = count(fences, group_id, bro)
  let count_tli = count(fences, group_id, tli)
  let count_tri = count(fences, group_id, tri)
  let count_bli = count(fences, group_id, bli)
  let count_bri = count(fences, group_id, bri)
  let count_checkerboard_tl_br = count(fences, group_id, checkerboard_tl_br)
  let count_checkerboard_tr_bl = count(fences, group_id, checkerboard_tr_bl)
  //io.debug(#(
  //  "TLO,LRO,BLO,BRO,TLI,TRI,BLI,BRI,CTL,CTR",
  //  count_tlo,
  //  count_tro,
  //  count_blo,
  //  count_bro,
  //  count_tli,
  //  count_tri,
  //  count_bli,
  //  count_bri,
  //  count_checkerboard_tl_br,
  //  count_checkerboard_tr_bl,
  //))
  count_tlo
  + count_tro
  + count_blo
  + count_bro
  + count_tli
  + count_tri
  + count_bli
  + count_bri
  + 2
  * { count_checkerboard_tl_br + count_checkerboard_tr_bl }
}

fn count(fences: Grid(Fence), group_id: Int, masks: List(Mask)) {
  fences
  |> grid.count(fn(_, f: Fence) {
    list.map2(f.ids, masks, fn(id: Int, mask: Mask) {
      case mask {
        Ne -> id != group_id
        Eq -> id == group_id
      }
    })
    |> list.all(fn(b) { b })
  })
}
