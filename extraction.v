Require Import VPHL.PrImp.
Require Import Sf.Maps.

Fixpoint ceval_step (st : state) (c : com) (i : nat) : option state :=
  match i with
    | O    => None
    | S i' => match c with
               | Skip => Some st
               | _ => Some st 
                 (* | l ::= a1 => *)
                 (*   Some (t_update st l (aeval st a1) )*)
                               
             end
  end.

Extraction "primp1.ml" ceval_step.

Extract Inductive bool => "bool" [ "true" "false" ].

(** Also, for non-enumeration types (where the constructors take
    arguments), we give an OCaml expression that can be used as a
    "recursor" over elements of the type.  (Think Church numerals.) *)

Extract Inductive nat => "int"
  [ "0" "(fun x -> x + 1)" ]
  "(fun zero succ n ->
      if n=0 then zero () else succ (n-1))".

(** We can also extract defined constants to specific OCaml terms or
    operators. *)

Extract Constant plus => "( + )".
Extract Constant mult => "( * )".
Extract Constant beq_nat => "( = )".

(** Important: It is entirely _your responsibility_ to make sure that
    the translations you're proving make sense.  For example, it might
    be tempting to include this one

      Extract Constant minus => "( - )".

    but doing so could lead to serious confusion!  (Why?)
*)

Extraction "imp2.ml" ceval_step.

(** Have a look at the file [imp2.ml].  Notice how the fundamental
    definitions have changed from [imp1.ml]. *)

(* ################################################################# *)
(** * A Complete Example *)

(** To use our extracted evaluator to run Imp programs, all we need to
    add is a tiny driver program that calls the evaluator and prints
    out the result.

    For simplicity, we'll print results by dumping out the first four
    memory locations in the final state.

    Also, to make it easier to type in examples, let's extract a
    parser from the [ImpParser] Coq module.  To do this, we need a few
    magic declarations to set up the right correspondence between Coq
    strings and lists of OCaml characters. *)

Require Import Ascii String.
Extract Inductive ascii => char
[
"(* If this appears, you're using Ascii internals. Please don't *) (fun (b0,b1,b2,b3,b4,b5,b6,b7) -> let f b i = if b then 1 lsl i else 0 in Char.chr (f b0 0 + f b1 1 + f b2 2 + f b3 3 + f b4 4 + f b5 5 + f b6 6 + f b7 7))"
]
"(* If this appears, you're using Ascii internals. Please don't *) (fun f c -> let n = Char.code c in let h i = (n land (1 lsl i)) <> 0 in f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))".
Extract Constant zero => "'\000'".
Extract Constant one => "'\001'".
Extract Constant shift =>
 "fun b c -> Char.chr (((Char.code c) lsl 1) land 255 + if b then 1 else 0)".
Extract Inlined Constant ascii_dec => "(=)".

(** We also need one more variant of booleans. *)

Extract Inductive sumbool => "bool" ["true" "false"].

(** The extraction is the same as always. *)

Require Import Imp.
Require Import ImpParser.
Extraction "imp.ml" empty_state ceval_step parse.

