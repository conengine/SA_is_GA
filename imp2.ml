type 'a option =
| Some of 'a
| None

type ('a, 'b) prod =
| Pair of 'a * 'b

type aid =
  int
  (* singleton inductive, whose constructor was Aid *)

type bid =
  int
  (* singleton inductive, whose constructor was Bid *)

type state = (aid -> int, bid -> bool) prod

type r (* AXIOM TO BE REALIZED *)

type aexp =
| ANum of int
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

(** val ceval_step : state -> com -> int -> state option **)

let rec ceval_step st _ i =
  (fun zero succ n ->       if n=0 then zero () else succ (n-1))
    (fun _ ->
    None)
    (fun _ -> Some
    st)
    i
