type bool =
| True
| False

val negb : bool -> bool

type nat =
| O
| S of nat

type 'a option =
| Some of 'a
| None

type ('a, 'b) prod =
| Pair of 'a * 'b

val fst : ('a1, 'a2) prod -> 'a1

val snd : ('a1, 'a2) prod -> 'a2

val add : nat -> nat -> nat

val mul : nat -> nat -> nat

val sub : nat -> nat -> nat

module Nat :
 sig
  val eqb : nat -> nat -> bool
 end

val ble_nat : nat -> nat -> bool

type aid =
  nat
  (* singleton inductive, whose constructor was Aid *)

val beq_aid : aid -> aid -> bool

type bid =
  nat
  (* singleton inductive, whose constructor was Bid *)

val beq_bid : bid -> bid -> bool

type state = (aid -> nat, bid -> bool) prod

val update : state -> aid -> nat -> state

val update_b : state -> bid -> bool -> state

type r (* AXIOM TO BE REALIZED *)

type aexp =
| ANum of nat
| AId of aid
| APlus of aexp * aexp
| AMinus of aexp * aexp
| AMult of aexp * aexp

val aeval : aexp -> state -> nat

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

val ceval_step : state -> com -> nat -> state option
