import extra
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/result
import gleam/set.{type Set}
import gleam/string

pub type Rule {
  Rule(first: Int, then: Int)
}

pub type Input {
  Input(rules: List(Rule), manuals: List(List(Int)))
}

pub fn parse(input: String) -> Input {
  case string.split(input, "\n\n") {
    [rules_str, manuals_str] -> {
      let rules =
        rules_str
        |> string.split("\n")
        |> list.map(fn(line) {
          case string.split(line, "|") {
            [first, then] ->
              Rule(first: extra.yolo_int(first), then: extra.yolo_int(then))
            _ -> panic as "Bad input 2!"
          }
        })
      let manuals =
        manuals_str
        |> string.split("\n")
        |> list.map(fn(manual) {
          manual
          |> string.split(",")
          |> list.map(extra.yolo_int)
        })
      Input(rules, manuals)
    }
    _ -> panic as "Bad input 1!"
  }
}

fn is_ok(manual: List(Int), dependents: Dict(Int, Set(Int))) {
  list.fold_until(manual, #(True, set.new()), fn(acc, page) {
    let #(ok, seen) = acc
    let dependents_ = dict.get(dependents, page) |> result.unwrap(set.new())
    case set.is_empty(set.intersection(dependents_, seen)) {
      True -> list.Continue(#(ok, set.insert(seen, page)))
      False -> list.Stop(#(False, seen))
    }
  })
  |> fn(acc) { acc.0 }
}

fn get_dependents(input: Input) -> Dict(Int, Set(Int)) {
  list.fold(input.rules, dict.new(), fn(acc, rule) {
    let acc_rule = dict.get(acc, rule.first) |> result.unwrap(set.new())
    let new_rule = set.insert(acc_rule, rule.then)
    dict.insert(acc, rule.first, new_rule)
  })
}

fn middle_page(manual: List(Int)) -> Result(Int, Nil) {
  let len = list.length(manual)
  let drop = len / 2

  manual
  |> list.drop(drop)
  |> list.first
}

pub fn pt_1(input: Input) {
  let dependents = get_dependents(input)
  input.manuals
  |> list.filter_map(fn(manual) {
    use <- bool.guard(when: !is_ok(manual, dependents), return: Error(Nil))
    middle_page(manual)
  })
  |> int.sum
}

fn fix_order(manual: List(Int), dependents: Dict(Int, Set(Int))) -> List(Int) {
  let manual_set = set.from_list(manual)
  fix_order_rec(
    dependents
      |> dict.filter(fn(k, _) { set.contains(manual_set, k) })
      |> dict.map_values(fn(_, v) { set.intersection(v, manual_set) })
      |> dict.to_list,
    [],
  )
  |> list.reverse
}

fn fix_order_rec(todos: List(#(Int, Set(Int))), acc: List(Int)) -> List(Int) {
  case list.is_empty(todos) {
    True -> acc
    False -> {
      let #(usable, rest) =
        extra.list_partition(todos, fn(todo_) { set.is_empty(todo_.1) })
      let usable_pages: List(Int) = usable |> list.map(fn(todo_) { todo_.0 })
      let cleaned_rest: List(#(Int, Set(Int))) =
        rest
        |> list.map(fn(todo_) {
          #(
            todo_.0,
            set.filter(todo_.1, fn(item) {
              !list.contains(usable_pages, any: item)
            }),
          )
        })
      let new_acc: List(Int) = list.append(acc, usable_pages)
      fix_order_rec(cleaned_rest, new_acc)
    }
  }
}

pub fn pt_2(input: Input) {
  let dependents = get_dependents(input)
  input.manuals
  |> list.filter_map(fn(manual) {
    use <- bool.guard(when: is_ok(manual, dependents), return: Error(Nil))
    middle_page(fix_order(manual, dependents))
  })
  |> int.sum
}
