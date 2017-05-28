type bool =
| True
| False

type nat =
| O
| S of nat

type 'a option =
| Some of 'a
| None

type ('a, 'b) prod =
| Pair of 'a * 'b

type aid =
  nat
  (* singleton inductive, whose constructor was Aid *)

type bid =
  nat
  (* singleton inductive, whose constructor was Bid *)

type state = (aid -> nat, bid -> bool) prod

type r (* AXIOM TO BE REALIZED *)

type aexp =
| ANum of nat
| AId of aid
| APlus of aexp * aexp
| AMinus of aexp * aexp
| AMult of aexp * aexp

type bexp =
| BTrue
| BFalse
| BId of bid
| BEq of aexp * aexp
| BLe of aexp * aexp
| BNot of bexp
| BAnd of bexp * bexp

type com =
| Skip
| Assign of aid * aexp
| BAssign of bid * bexp
| Seq of com * com
| If of bid * com * com
| While of bid * com
| Toss of r * aid
| BToss of r * bid

(** val ceval_step : state -> com -> nat -> state option **)

let rec ceval_step st _ = function
| O -> None
| S _ -> Some st
