! Copyright (C) 2022 Martin Janiczek.
! See http://factorcode.org/license.txt for BSD license.
USING: kernel sequences io.encodings.utf8 io.files io peg.ebnf
multiline math.parser strings accessors combinators match math
assocs arrays system prettyprint ;
IN: aoc-2016-12

EBNF: parse [=[

Register = [a-d]       => [[ 1string ]]
Literal  = "-"? [0-9]+ => [[ concat string>number ]]
Value = Register | Literal

Cpy = "cpy" " "~ Value " "~ Register
Inc = "inc" " "~ Register
Dec = "dec" " "~ Register
Jnz = "jnz" " "~ Value " "~ Literal

Operation = (Cpy | Inc | Dec | Jnz) ("\n"?)~

Main = Operation+

]=]

TUPLE: state ops ip a b c d ;

: <state> ( ops -- state )
  state new 
  swap >>ops
  0 >>ip
  0 >>a 0 >>b 0 >>c 0 >>d ;

: change-register! ( state fn reg -- state )
  {
    { "a" [ change-a ] }
    { "b" [ change-b ] }
    { "c" [ change-c ] }
    { "d" [ change-d ] }
  } at call( state fn -- state ) ;

: set-register! ( state val reg -- state )
  {
    { "a" [ >>a ] }
    { "b" [ >>b ] }
    { "c" [ >>c ] }
    { "d" [ >>d ] }
  } at call( state fn -- state ) ;

: get-val ( state val -- int )
  {
    { [ dup "a" = ]   [ drop a>> ] }
    { [ dup "b" = ]   [ drop b>> ] }
    { [ dup "c" = ]   [ drop c>> ] }
    { [ dup "d" = ]   [ drop d>> ] }
    { [ dup number? ] [ nip ] }
  } cond
  ;

: inc-ip! ( state -- state )
  [ 1 + ] change-ip ;

: ip-in-bounds? ( state -- ? )
  [ ip>> ] [ ops>> ] bi bounds-check? ;

: get-op ( state -- op )
  [ ip>> ] [ ops>> ] bi nth ;

MATCH-VARS: ?val ?lit ?reg ?other ;

: step ( state -- state ? )
  dup get-op
  {
    { { "cpy" ?val ?reg } [ dup ?val get-val ?reg set-register! inc-ip! ] }
    { { "inc" ?reg }      [ [ 1 + ] ?reg change-register! inc-ip! ] }
    { { "dec" ?reg }      [ [ 1 - ] ?reg change-register! inc-ip! ] }
    { { "jnz" ?val ?lit } [ dup ?val get-val zero? 
                            [ inc-ip! ] [ [ ?lit + ] change-ip ] if ] }
  } match-cond
  dup ip-in-bounds? ;

: common ( filename -- state )
  "/Applications/factor/work/aoc-2016-12/" prepend
  utf8 file-contents parse <state> ;

: part1 ( filename -- int-answer ) common       [ step ] loop a>> ;
: part2 ( filename -- int-answer ) common 1 >>c [ step ] loop a>> ;
: ex1 ( -- int-answer ) "example.in" part1 ;
: p1  ( -- int-answer ) "input.in"   part1 ;
: p2  ( -- int-answer ) "input.in"   part2 ;
: main ( -- ) p1 . flush p2 . flush ;

MAIN: main
