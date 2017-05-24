Module Type Sig.
  Parameter A Opt:Type.
  Parameter le : A -> A -> Prop.
  Parameter is_SA is_GA : Opt -> Prop.
  Infix "<=" := le : order_scope.
  Open Scope order_scope.
  
  Axiom sa_is_ga: forall opt, is_SA opt -> is_GA opt.
  Axiom ga_is_sa: forall opt, is_GA opt -> is_SA opt.
End Sig.

