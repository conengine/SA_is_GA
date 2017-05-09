Module Type ENG.
  Parameter Dob : Type.
  (* Parameter human : Set. *)
  Parameter data : Set.
  Parameter le : Dob -> Dob -> Prop.
  Parameter bayes_update : Dob -> data -> Dob.
  Infix "⊑" := le (left associativity, at level 50).
  (* Local Notation "x ⊑ y" :=  (le x y). *)
  Axiom monotone_util : forall obs dob, (bayes_update dob obs) ⊑ dob.
End ENG.

Module Type ENG_THEOREMS.
  Declare Module E : ENG. 
  