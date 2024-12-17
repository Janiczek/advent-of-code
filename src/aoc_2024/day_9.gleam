import extra
import gleam/deque.{type Deque}
import gleam/int
import gleam/list
import gleam/order
import gleam/string
import pocket_watch

pub type Block {
  Block(id: Int, length: Int)
}

pub fn parse(input: String) -> Deque(Block) {
  use <- pocket_watch.simple("parse")
  input
  |> string.to_graphemes
  |> list.index_map(fn(len, i) {
    Block(
      id: case i % 2 == 0 {
        True -> i / 2
        False -> -1
      },
      length: extra.yolo_int(len),
    )
  })
  |> list.filter(fn(b) { b.length > 0 })
  |> deque.from_list
}

pub fn pt_1(input: Deque(Block)) {
  use <- pocket_watch.simple("part 1")
  pt_1_fix(todos: input, done: [])
  |> checksum
}

fn is_free(block: Block) -> Bool {
  block.id == -1
}

fn pt_1_fix(
  todos todos: Deque(Block),
  done done_rev: List(Block),
) -> List(Block) {
  case deque.pop_front(todos) {
    Error(Nil) ->
      // no blocks (free / data) left
      list.reverse(done_rev)
    Ok(#(first, rest)) ->
      case is_free(first) {
        False ->
          // Pop that data into done!
          pt_1_fix(todos: rest, done: [first, ..done_rev])
        True -> {
          let free_length = first.length
          case free_length == 0 {
            True ->
              // Ignore empty free block
              pt_1_fix(todos: rest, done: done_rev)
            False ->
              case deque.pop_back(rest) {
                Error(Nil) ->
                  // free blocks left, but no data left
                  list.reverse(done_rev)
                Ok(#(last_block, rest2)) -> {
                  case is_free(last_block) {
                    True -> {
                      // free block at the end - ignore
                      pt_1_fix(
                        todos: rest2
                          |> deque.push_front(first),
                        done: done_rev,
                      )
                    }
                    False -> {
                      // do just one cell instead of two intervals
                      let Block(id, length) = last_block
                      let should_push_data_to_back = length > 1
                      let should_push_free_to_front = free_length > 1
                      let new_todos =
                        rest2
                        |> extra.do_if(should_push_data_to_back, fn(q) {
                          deque.push_back(q, Block(id, length - 1))
                        })
                        |> extra.do_if(should_push_free_to_front, fn(q) {
                          deque.push_front(q, Block(-1, free_length - 1))
                        })
                      pt_1_fix(todos: new_todos, done: [
                        Block(id: id, length: 1),
                        ..done_rev
                      ])
                    }
                  }
                }
              }
          }
        }
      }
  }
}

fn checksum(blocks: List(Block)) {
  list.fold(over: blocks, from: #(0, 0), with: fn(acc, block) {
    let #(i, csum) = acc
    case is_free(block) {
      True -> #(i + block.length, csum)
      False -> #(
        i + block.length,
        csum
          + {
          list.range(i, i + block.length - 1)
          |> list.map(fn(ii) { ii * block.id })
          |> int.sum
        },
      )
    }
  })
  |> fn(acc) { acc.1 }
}

pub fn pt_2(input: Deque(Block)) {
  use <- pocket_watch.simple("part 2")
  let final = pt_2_fix(input, todos: input)
  checksum(final)
}

fn pt_2_fix(input: Deque(Block), todos todos: Deque(Block)) -> List(Block) {
  case deque.pop_back(todos) {
    Error(Nil) ->
      // no blocks (free / data) left
      deque.to_list(input)
    Ok(#(todo_, rest)) ->
      case is_free(todo_) {
        True ->
          // A free space at the end - ignore it. Don't remove it from the input, it might be needed.
          pt_2_fix(input, rest)
        False -> {
          let Block(id, length) = todo_
          case pt_2_fix_specific(id, length, [], input) {
            Error(Nil) ->
              // Didn't find a large enough place for this item - keep it at the end
              pt_2_fix(input, rest)
            Ok(new_input) -> pt_2_fix(new_input, rest)
          }
        }
      }
  }
}

/// OK = we have moved it somewhere. It's no longer at the end, it's in the middle
fn pt_2_fix_specific(
  id: Int,
  length: Int,
  start_rev: List(Block),
  input: Deque(Block),
) -> Result(Deque(Block), Nil) {
  // go over the whole input from the start, and find a free space
  case deque.pop_front(input) {
    Error(Nil) -> Error(Nil)
    Ok(#(first, rest)) ->
      case is_free(first) {
        False ->
          case first == Block(id: id, length: length) {
            False ->
              // Continue search for a free space
              pt_2_fix_specific(id, length, [first, ..start_rev], rest)
            True ->
              // We got to the original element, we can short circuit. Not found
              Error(Nil)
          }
        True -> {
          case int.compare(length, first.length) {
            order.Lt -> {
              // More space than needed
              Ok(
                list.append(
                  list.reverse([
                    Block(id: -1, length: first.length - length),
                    Block(id: id, length: length),
                    ..start_rev
                  ]),
                  deque.to_list(
                    rest
                    |> deque_free_first_from_back(Block(id: id, length: length)),
                  ),
                )
                |> compact
                |> deque.from_list,
              )
            }
            order.Eq -> {
              // Just enough space
              Ok(
                list.append(
                  list.reverse([Block(id: id, length: length), ..start_rev]),
                  deque.to_list(
                    rest
                    |> deque_free_first_from_back(Block(id: id, length: length)),
                  ),
                )
                |> compact
                |> deque.from_list,
              )
            }
            order.Gt ->
              // Not enough space
              pt_2_fix_specific(id, length, [first, ..start_rev], rest)
          }
        }
      }
  }
}

fn deque_free_first_from_back(q: Deque(Block), item: Block) -> Deque(Block) {
  deque_free_first_from_back_aux(q, item, [])
}

fn deque_free_first_from_back_aux(
  q: Deque(Block),
  item: Block,
  acc: List(Block),
) -> Deque(Block) {
  case deque.pop_back(q) {
    Error(Nil) -> deque.from_list(acc)
    Ok(#(a, q2)) ->
      case a == item {
        True -> {
          deque.from_list(list.append(
            deque.to_list(
              q2
              |> deque.push_back(Block(id: -1, length: a.length)),
            ),
            acc,
          ))
        }
        False -> deque_free_first_from_back_aux(q2, item, [a, ..acc])
      }
  }
}

fn compact(bs: List(Block)) -> List(Block) {
  case bs {
    [] -> bs
    [first, ..rest] -> compact_help(start_rev: [], current: first, rest: rest)
  }
}

fn compact_help(
  start_rev start_rev: List(Block),
  current current: Block,
  rest rest: List(Block),
) -> List(Block) {
  case is_free(current) {
    False ->
      case rest {
        [] -> list.reverse(start_rev)
        [next, ..rest2] -> compact_help([current, ..start_rev], next, rest2)
      }
    True ->
      case rest {
        [] -> list.reverse([current, ..start_rev])
        [next, ..rest2] ->
          case is_free(next) {
            True ->
              compact_help(
                start_rev,
                Block(id: -1, length: current.length + next.length),
                rest2,
              )
            False -> compact_help([current, ..start_rev], next, rest2)
          }
      }
  }
}
