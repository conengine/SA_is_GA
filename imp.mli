val negb : bool -> bool

type 'a option =
| Some of 'a
| None

type ('a, 'b) prod =
| Pair of 'a * 'b

val fst : ('a1, 'a2) prod -> 'a1

val snd : ('a1, 'a2) prod -> 'a2

val add : int -> int -> int

val mul : int -> int -> int

val sub : int -> int -> int

module Nat :
 sig
  val eqb : int -> int -> bool
 end

val ble_nat : int -> int -> bool

type aid =
  int
  (* singleton inductive, whose constructor was Aid *)

val beq_aid : aid -> aid -> bool

type bid =
  int
  (* singleton inductive, whose constructor was Bid *)

val beq_bid : bid -> bid -> bool

type state = (aid -> int, bid -> bool) prod

val empty_state : state

val update : state -> aid -> int -> state

val update_b : state -> bid -> bool -> state

type r (* AXIOM TO BE REALIZED *)

type aexp =
| ANum of int
| AId of aid
| APlus of aexp * aexp
| AMinus of aexp * aexp
| AMult of aexp * aexp

val aeval : aexp -> state -> int

type bexp =
| BTrue
| BFalse
| BId of bid
| BEq of aexp * aexp
| BLe of aexp * aexp
| BNot of bexp
| BAnd of bexp * bexp

val beval : bexp -> state -> bool

type com =
| Skip
| Assign of aid * aexp
| BAssign of bid * bexp
| Seq of com * com
| If of bid * com * com
| While of bid * com
| Toss of r * aid
| BToss of r * bid

val ceval_step : state -> com -> int -> state option
