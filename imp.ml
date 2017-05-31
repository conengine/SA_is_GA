(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

type 'a option =
| Some of 'a
| None

type ('a, 'b) prod =
| Pair of 'a * 'b

(** val fst : ('a1, 'a2) prod -> 'a1 **)

let fst = function
| Pair (x, _) -> x

(** val snd : ('a1, 'a2) prod -> 'a2 **)

let snd = function
| Pair (_, y) -> y

(** val add : int -> int -> int **)

let rec add = ( + )

(** val mul : int -> int -> int **)

let rec mul = ( * )

(** val sub : int -> int -> int **)

let rec sub n m =
  (fun zero succ n ->       if n=0 then zero () else succ (n-1))
    (fun _ ->
    n)
    (fun k ->
    (fun zero succ n ->       if n=0 then zero () else succ (n-1))
      (fun _ ->
      n)
      (fun l ->
      sub k l)
      m)
    n

module Nat =
 struct
  (** val eqb : int -> int -> bool **)

  let rec eqb = ( = )
 end

(** val ble_nat : int -> int -> bool **)

let rec ble_nat n m =
  (fun zero succ n ->       if n=0 then zero () else succ (n-1))
    (fun _ ->
    true)
    (fun n' ->
    (fun zero succ n ->       if n=0 then zero () else succ (n-1))
      (fun _ ->
      false)
      (fun m' ->
      ble_nat n' m')
      m)
    n

type aid =
  int
  (* singleton inductive, whose constructor was Aid *)

(** val beq_aid : aid -> aid -> bool **)

let beq_aid i1 i2 =
  Nat.eqb i1 i2

type bid =
  int
  (* singleton inductive, whose constructor was Bid *)

(** val beq_bid : bid -> bid -> bool **)

let beq_bid i1 i2 =
  Nat.eqb i1 i2

type state = (aid -> int, bid -> bool) prod

(** val empty_state : state **)

let empty_state =
  Pair ((fun _ -> 0), (fun _ -> false))

(** val update : state -> aid -> int -> state **)

let update st x n =
  Pair ((fun x' -> if beq_aid x x' then n else fst st x'), (snd st))

(** val update_b : state -> bid -> bool -> state **)

let update_b st y b =
  Pair ((fst st), (fun y' -> if beq_bid y y' then b else snd st y'))

type r (* AXIOM TO BE REALIZED *)

type aexp =
| ANum of int
| AId of aid
| APlus of aexp * aexp
| AMinus of aexp * aexp
| AMult of aexp * aexp

(** val aeval : aexp -> state -> int **)

let rec aeval a st =
  match a with
  | ANum n -> n
  | AId x -> fst st x
  | APlus (a1, a2) -> add (aeval a1 st) (aeval a2 st)
  | AMinus (a1, a2) -> sub (aeval a1 st) (aeval a2 st)
  | AMult (a1, a2) -> mul (aeval a1 st) (aeval a2 st)

type bexp =
| BTrue
| BFalse
| BId of bid
| BEq of aexp * aexp
| BLe of aexp * aexp
| BNot of bexp
| BAnd of bexp * bexp

(** val beval : bexp -> state -> bool **)

let rec beval b st =
  match b with
  | BTrue -> true
  | BFalse -> false
  | BId y -> snd st y
  | BEq (a1, a2) -> Nat.eqb (aeval a1 st) (aeval a2 st)
  | BLe (a1, a2) -> ble_nat (aeval a1 st) (aeval a2 st)
  | BNot b1 -> negb (beval b1 st)
  | BAnd (b1, b2) -> if beval b1 st then beval b2 st else false

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

let rec ceval_step st c i =
  (fun zero succ n ->       if n=0 then zero () else succ (n-1))
    (fun _ ->
    None)
    (fun i' ->
    match c with
    | Skip -> Some st
    | Assign (l, a1) -> Some (update st l (aeval a1 st))
    | BAssign (l, b1) -> Some (update_b st l (beval b1 st))
    | Seq (c1, c2) ->
      (match ceval_step st c1 i' with
       | Some st' -> ceval_step st' c2 i'
       | None -> None)
    | BToss (_, y) -> Some (update_b st y true)
    | _ -> None)
    i
