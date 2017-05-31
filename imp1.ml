type bool =
| True
| False

(** val negb : bool -> bool **)

let negb = function
| True -> False
| False -> True

type nat =
| O
| S of nat

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

(** val add : nat -> nat -> nat **)

let rec add n m =
  match n with
  | O -> m
  | S p -> S (add p m)

(** val mul : nat -> nat -> nat **)

let rec mul n m =
  match n with
  | O -> O
  | S p -> add m (mul p m)

(** val sub : nat -> nat -> nat **)

let rec sub n m =
  match n with
  | O -> n
  | S k ->
    (match m with
     | O -> n
     | S l -> sub k l)

module Nat =
 struct
  (** val eqb : nat -> nat -> bool **)

  let rec eqb n m =
    match n with
    | O ->
      (match m with
       | O -> True
       | S _ -> False)
    | S n' ->
      (match m with
       | O -> False
       | S m' -> eqb n' m')
 end

(** val ble_nat : nat -> nat -> bool **)

let rec ble_nat n m =
  match n with
  | O -> True
  | S n' ->
    (match m with
     | O -> False
     | S m' -> ble_nat n' m')

type aid =
  nat
  (* singleton inductive, whose constructor was Aid *)

(** val beq_aid : aid -> aid -> bool **)

let beq_aid i1 i2 =
  Nat.eqb i1 i2

type bid =
  nat
  (* singleton inductive, whose constructor was Bid *)

(** val beq_bid : bid -> bid -> bool **)

let beq_bid i1 i2 =
  Nat.eqb i1 i2

type state = (aid -> nat, bid -> bool) prod

(** val update : state -> aid -> nat -> state **)

let update st x n =
  Pair ((fun x' ->
    match beq_aid x x' with
    | True -> n
    | False -> fst st x'), (snd st))

(** val update_b : state -> bid -> bool -> state **)

let update_b st y b =
  Pair ((fst st), (fun y' ->
    match beq_bid y y' with
    | True -> b
    | False -> snd st y'))

type r (* AXIOM TO BE REALIZED *)

type aexp =
| ANum of nat
| AId of aid
| APlus of aexp * aexp
| AMinus of aexp * aexp
| AMult of aexp * aexp

(** val aeval : aexp -> state -> nat **)

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
  | BTrue -> True
  | BFalse -> False
  | BId y -> snd st y
  | BEq (a1, a2) -> Nat.eqb (aeval a1 st) (aeval a2 st)
  | BLe (a1, a2) -> ble_nat (aeval a1 st) (aeval a2 st)
  | BNot b1 -> negb (beval b1 st)
  | BAnd (b1, b2) ->
    (match beval b1 st with
     | True -> beval b2 st
     | False -> False)

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

let rec ceval_step st c = function
| O -> None
| S i' ->
  (match c with
   | Skip -> Some st
   | Assign (l, a1) -> Some (update st l (aeval a1 st))
   | BAssign (l, b1) -> Some (update_b st l (beval b1 st))
   | Seq (c1, c2) ->
     (match ceval_step st c1 i' with
      | Some st' -> ceval_step st' c2 i'
      | None -> None)
   | BToss (_, y) -> Some (update_b st y True)
   | _ -> None)
