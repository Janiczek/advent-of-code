import extra
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import pocket_watch

pub type Input {
  Input(mem: Dict(Int, Int), reg_a: Int, reg_b: Int, reg_c: Int)
}

pub type Op {
  // The adv instruction (opcode 0) performs division. The numerator is the value
  // in the A register. The denominator is found by raising 2 to the power of the
  // instruction's combo operand. (So, an operand of 2 would divide A by 4 (2^2);
  // an operand of 5 would divide A by 2^B.) The result of the division operation
  // is truncated to an integer and then written to the A register.
  Adv

  // The bxl instruction (opcode 1) calculates the bitwise XOR of register B and
  // the instruction's literal operand, then stores the result in register B.
  Bxl

  // The bst instruction (opcode 2) calculates the value of its combo operand
  // modulo 8 (thereby keeping only its lowest 3 bits), then writes that value to
  // the B register.
  Bst

  // The jnz instruction (opcode 3) does nothing if the A register is 0. However,
  // if the A register is not zero, it jumps by setting the instruction pointer
  // to the value of its literal operand; if this instruction jumps, the
  // instruction pointer is not increased by 2 after this instruction.
  Jnz

  // The bxc instruction (opcode 4) calculates the bitwise XOR of register B and
  // register C, then stores the result in register B. (For legacy reasons, this
  // instruction reads an operand but ignores it.)
  Bxc

  // The out instruction (opcode 5) calculates the value of its combo operand
  // modulo 8, then outputs that value. (If a program outputs multiple values,
  // they are separated by commas.)
  Out

  // The bdv instruction (opcode 6) works exactly like the adv instruction except
  // that the result is stored in the B register. (The numerator is still read
  // from the A register.)
  Bdv

  // The cdv instruction (opcode 7) works exactly like the adv instruction except
  // that the result is stored in the C register. (The numerator is still read
  // from the A register.)
  Cdv
}

pub fn parse(input: String) -> Input {
  use <- pocket_watch.simple("parse")
  case string.split(input, "\n\n") {
    [regs, "Program: " <> program] -> {
      case string.split(regs, "\n") {
        [
          "Register A: " <> reg_a,
          "Register B: " <> reg_b,
          "Register C: " <> reg_c,
        ] -> {
          Input(
            mem: program
              |> string.split(",")
              |> list.map(extra.yolo_int)
              |> list.index_map(fn(x, i) { #(i, x) })
              |> dict.from_list,
            reg_a: extra.yolo_int(reg_a),
            reg_b: extra.yolo_int(reg_b),
            reg_c: extra.yolo_int(reg_c),
          )
        }
        _ -> panic as "bad input? 2"
      }
    }
    _ -> panic as "bad input?"
  }
}

fn parse_op(input: Int) -> Op {
  case input {
    0 -> Adv
    1 -> Bxl
    2 -> Bst
    3 -> Jnz
    4 -> Bxc
    5 -> Out
    6 -> Bdv
    7 -> Cdv
    _ -> panic as "bad input? 3"
  }
}

pub fn pt_1(input: Input) {
  use <- pocket_watch.simple("part 1")

  step(input, 0, [])
  |> list.reverse
}

fn step(input: Input, ip: Int, out_rev: List(Int)) {
  case dict.get(input.mem, ip) {
    Error(Nil) -> out_rev
    Ok(n) -> {
      let op = parse_op(n)
      case op {
        Adv -> adv(input, ip, out_rev)
        Bxl -> bxl(input, ip, out_rev)
        Bst -> bst(input, ip, out_rev)
        Jnz -> jnz(input, ip, out_rev)
        Bxc -> bxc(input, ip, out_rev)
        Out -> out(input, ip, out_rev)
        Bdv -> bdv(input, ip, out_rev)
        Cdv -> cdv(input, ip, out_rev)
      }
    }
  }
}

type Reg {
  RegA
  RegB
  RegC
}

fn adv(input: Input, ip: Int, out_rev: List(Int)) {
  division(input, ip, out_rev, RegA)
}

fn bdv(input: Input, ip: Int, out_rev: List(Int)) {
  division(input, ip, out_rev, RegB)
}

fn cdv(input: Input, ip: Int, out_rev: List(Int)) {
  division(input, ip, out_rev, RegC)
}

fn bxl(input: Input, ip: Int, out_rev: List(Int)) {
  let xor = int.bitwise_exclusive_or(input.reg_b, literal(input.mem, ip + 1))
  let new_input = write_to_reg(input, RegB, xor)
  step(new_input, ip + 2, out_rev)
}

fn bst(input: Input, ip: Int, out_rev: List(Int)) {
  let new_input = write_to_reg(input, RegB, combo(input, ip + 1) % 8)
  step(new_input, ip + 2, out_rev)
}

fn jnz(input: Input, ip: Int, out_rev: List(Int)) {
  case input.reg_a == 0 {
    True -> step(input, ip + 2, out_rev)
    False -> step(input, literal(input.mem, ip + 1), out_rev)
  }
}

fn bxc(input: Input, ip: Int, out_rev: List(Int)) {
  let xor = int.bitwise_exclusive_or(input.reg_b, input.reg_c)
  let new_input = write_to_reg(input, RegB, xor)
  step(new_input, ip + 2, out_rev)
}

fn out(input: Input, ip: Int, out_rev: List(Int)) {
  let new_out = combo(input, ip + 1) % 8
  step(input, ip + 2, [new_out, ..out_rev])
}

fn division(input: Input, ip: Int, out_rev: List(Int), reg: Reg) {
  let num = input.reg_a
  let den = extra.pow(2, combo(input, ip + 1))
  let result = num / den
  let new_input = write_to_reg(input, reg, result)
  step(new_input, ip + 2, out_rev)
}

fn combo(input: Input, ip: Int) -> Int {
  case dict.get(input.mem, ip) {
    Error(Nil) -> panic as "combo OOB"
    Ok(val) ->
      case val {
        0 | 1 | 2 | 3 -> val
        4 -> input.reg_a
        5 -> input.reg_b
        6 -> input.reg_c
        7 -> panic as "combo 7"
        _ -> panic as "combo >7"
      }
  }
}

fn literal(mem: Dict(Int, Int), ip: Int) -> Int {
  case dict.get(mem, ip) {
    Error(Nil) -> panic as "literal OOB"
    Ok(val) -> val
  }
}

fn write_to_reg(input: Input, reg: Reg, value: Int) -> Input {
  case reg {
    RegA -> Input(..input, reg_a: value)
    RegB -> Input(..input, reg_b: value)
    RegC -> Input(..input, reg_c: value)
  }
}

pub fn pt_2(input: Input) {
  use <- pocket_watch.simple("part 2")
  let wanted_out =
    input.mem
    |> dict.to_list
    |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
    |> list.map(fn(kv) { kv.1 })
  let wanted_out_rev = list.reverse(wanted_out)
  find_out(
    wanted_out_rev,
    list.length(wanted_out_rev),
    input,
    // 1
    1_074_137_000_837_131,
    // pow(8,11)
    1,
    0,
    0,
  )
}

fn find_out(
  wanted_out_rev: List(Int),
  wanted_out_len: Int,
  input: Input,
  i: Int,
  d: Int,
  best_i: Int,
  best_match: Int,
) {
  let actual_out_rev = step(Input(..input, reg_a: i), 0, [])
  let actual_out_len = list.length(actual_out_rev)
  use <- bool.guard(when: wanted_out_rev == actual_out_rev, return: i)
  case wanted_out_len == actual_out_len {
    False ->
      find_out(
        wanted_out_rev,
        wanted_out_len,
        input,
        i + d,
        d,
        best_i,
        best_match,
      )
    True -> {
      let matches = matching_out(wanted_out_rev, actual_out_rev, 0)
      case matches > best_match {
        True -> {
          io.println(
            string.inspect(#("Found new best", i, matches, actual_out_rev)),
          )
          find_out(wanted_out_rev, wanted_out_len, input, i + d, d, i, matches)
        }
        False ->
          find_out(
            wanted_out_rev,
            wanted_out_len,
            input,
            i + d,
            d,
            best_i,
            best_match,
          )
      }
    }
  }
}

fn matching_out(xs: List(Int), ys: List(Int), acc: Int) {
  case xs, ys {
    [x, ..xs_], [y, ..ys_] if x == y -> matching_out(xs_, ys_, acc + 1)
    _, _ -> acc
  }
}
