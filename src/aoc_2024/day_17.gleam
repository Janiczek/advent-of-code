import extra
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/order
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
  io.debug(input)
  let out = step(input, 0, [])
  io.println_error(
    out
    |> list.map(int.to_string)
    |> string.join(","),
  )
  out
}

pub fn pt_2(input: Input) {
  use <- pocket_watch.simple("part 2")
  let wanted_out =
    input.mem
    |> dict.to_list
    |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
    |> list.map(fn(kv) { kv.1 })
  let wanted_length = list.length(wanted_out)
  find_out(wanted_out, wanted_length, input, 1)
}

fn step(input: Input, ip: Int, out_rev: List(Int)) {
  case dict.get(input.mem, ip) {
    Error(Nil) -> list.reverse(out_rev)
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
  let den = power_of_2(combo(input, ip + 1))
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

fn power_of_2(n: Int) -> Int {
  let assert Ok(result) = int.power(2, int.to_float(n))
  float.truncate(result)
}

fn find_out(wanted_out: List(Int), wanted_length: Int, input: Input, i: Int) {
  let actual_out = step(Input(..input, reg_a: i), 0, [])
  let actual_length = list.length(actual_out)
  case int.compare(actual_length, wanted_length) {
    order.Lt -> {
      io.debug(#("order", i, "up", actual_length))
      find_out(wanted_out, wanted_length, input, i * 8)
    }
    order.Eq -> {
      let actual_rev = list.reverse(actual_out)
      let wanted_rev = list.reverse(wanted_out)
      io.debug(#("same", actual_rev, wanted_rev, i))
      todo as "estimate the order of magnitude to move by (8^some_i)"
    }
    order.Gt -> {
      io.debug(#("order", i, "down", actual_length))
      find_out(wanted_out, wanted_length, input, i / 8)
    }
  }
}
