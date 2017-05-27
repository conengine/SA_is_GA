Require Import PrImp.
Require Import Maps.

Fixpoint ceval_step (st : state) (c : com) (i : nat) : option state :=
  match i with
    | O    => None
    | S i' => match c with
                 | Skip => Some st
                 | l ::= a1 =>
                   Some (t_update st l (aeval st a1))
  end
end.
                                                            
         