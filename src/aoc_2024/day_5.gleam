import extra
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/order
import gleam/result
import gleam/set.{type Set}
import gleam/string
import pocket_watch

pub type Rule {
  Rule(first: Int, then: Int)
}

pub type Input {
  Input(rules: List(Rule), manuals: List(List(Int)))
}

pub fn parse(input: String) -> Input {
  use <- pocket_watch.simple("parse")
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
  use <- pocket_watch.simple("part 1")
  let dependents = get_dependents(input)
  input.manuals
  |> list.filter_map(fn(manual) {
    use <- bool.guard(when: !is_ok(manual, dependents), return: Error(Nil))
    middle_page(manual)
  })
  |> int.sum
}

fn fix_order(manual: List(Int), dependents: Dict(Int, Set(Int))) -> List(Int) {
  list.sort(manual, by: fn(a, b) {
    let a_dependents = dict.get(dependents, a) |> result.unwrap(set.new())
    case set.contains(a_dependents, b) {
      True -> order.Lt
      False -> {
        let b_dependents = dict.get(dependents, b) |> result.unwrap(set.new())
        case set.contains(b_dependents, a) {
          True -> order.Gt
          False -> order.Eq
        }
      }
    }
  })
}

pub fn pt_2(input: Input) {
  use <- pocket_watch.simple("part 2")
  let dependents = get_dependents(input)
  input.manuals
  |> list.filter_map(fn(manual) {
    use <- bool.guard(when: is_ok(manual, dependents), return: Error(Nil))
    middle_page(fix_order(manual, dependents))
  })
  |> int.sum
}
