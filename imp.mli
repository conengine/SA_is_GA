type unit0 =
| Tt

type 'a option =
| Some of 'a
| None

type ('a, 'b) prod =
| Pair of 'a * 'b

val fst : ('a1, 'a2) prod -> 'a1

val snd : ('a1, 'a2) prod -> 'a2

type 'a list =
| Nil
| Cons of 'a * 'a list

val app : 'a1 list -> 'a1 list -> 'a1 list

val add : int -> int -> int

val mul : int -> int -> int

val sub : int -> int -> int

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

module Nat :
 sig
  val eqb : int -> int -> bool

  val leb : int -> int -> bool
 end

module Pos :
 sig
  val succ : positive -> positive

  val add : positive -> positive -> positive

  val add_carry : positive -> positive -> positive

  val mul : positive -> positive -> positive

  val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1

  val to_nat : positive -> int
 end

module N :
 sig
  val add : n -> n -> n

  val mul : n -> n -> n

  val to_nat : n -> int
 end

val rev : 'a1 list -> 'a1 list

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1

val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1

val forallb : ('a1 -> bool) -> 'a1 list -> bool

val n_of_digits : bool list -> n

val n_of_ascii : char -> n

val nat_of_ascii : char -> int

type string =
| EmptyString
| String of char * string

val string_dec : string -> string -> bool

val append : string -> string -> string

type aid =
  int
  (* singleton inductive, whose constructor was Aid *)

val beq_aid : aid -> aid -> bool

type bid =
  int
  (* singleton inductive, whose constructor was Bid *)

type state = (aid -> int, bid -> bool) prod

val update : state -> aid -> int -> state

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

type com =
| Skip
| Assign of aid * aexp
| BAssign of bid * bexp
| Seq of com * com
| If of bid * com * com
| While of bid * com
| Toss of r * aid
| BToss of r * bid

type id =
  string
  (* singleton inductive, whose constructor was Id *)

type 'a total_map = id -> 'a

val t_empty : 'a1 -> 'a1 total_map

type state0 = int total_map

val empty_state : state0

type aexp0 =
| ANum0 of int
| AId0 of id
| APlus0 of aexp0 * aexp0
| AMinus0 of aexp0 * aexp0
| AMult0 of aexp0 * aexp0

type bexp0 =
| BTrue0
| BFalse0
| BEq0 of aexp0 * aexp0
| BLe0 of aexp0 * aexp0
| BNot0 of bexp0
| BAnd0 of bexp0 * bexp0

type com0 =
| CSkip
| CAss of id * aexp0
| CSeq of com0 * com0
| CIf of bexp0 * com0 * com0
| CWhile of bexp0 * com0

val isWhite : char -> bool

val isLowerAlpha : char -> bool

val isAlpha : char -> bool

val isDigit : char -> bool

type chartype =
| White
| Alpha
| Digit
| Other

val classifyChar : char -> chartype

val list_of_string : string -> char list

val string_of_list : char list -> string

type token = string

val tokenize_helper : chartype -> char list -> char list -> char list list

val tokenize : string -> string list

type 'x optionE =
| SomeE of 'x
| NoneE of string

type 't parser0 = token list -> ('t, token list) prod optionE

val many_helper :
  'a1 parser0 -> 'a1 list -> int -> token list -> ('a1 list, token list) prod
  optionE

val many : 'a1 parser0 -> int -> 'a1 list parser0

val firstExpect : token -> 'a1 parser0 -> 'a1 parser0

val expect : token -> unit0 parser0

val parseIdentifier : token list -> (id, token list) prod optionE

val parseNumber : token list -> (int, token list) prod optionE

val parsePrimaryExp : int -> token list -> (aexp0, token list) prod optionE

val parseProductExp : int -> token list -> (aexp0, token list) prod optionE

val parseSumExp : int -> token list -> (aexp0, token list) prod optionE

val parseAExp : int -> token list -> (aexp0, token list) prod optionE

val parseAtomicExp : int -> token list -> (bexp0, token list) prod optionE

val parseConjunctionExp :
  int -> token list -> (bexp0, token list) prod optionE

val parseBExp : int -> token list -> (bexp0, token list) prod optionE

val parseSimpleCommand : int -> token list -> (com0, token list) prod optionE

val parseSequencedCommand :
  int -> token list -> (com0, token list) prod optionE

val bignumber : int

val parse : string -> (com0, token list) prod optionE

val ceval_step : state -> com -> int -> state option
