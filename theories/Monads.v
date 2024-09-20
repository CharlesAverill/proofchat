Require Import String.

Inductive optionE (X:Type) : Type :=
  | SomeE (x : X)
  | NoneE (s : string).
Arguments SomeE {X}.
Arguments NoneE {X}.

Declare Scope monad_scope.

Notation " x <- e1 ;; e2" := (match e1 with
                              | SomeE x => e2
                              | NoneE err => NoneE err
                              end)
         (right associativity, at level 60) : monad_scope.
Notation " 'return' e "
  := (SomeE e) (at level 60) : monad_scope.
Notation " 'fail' s "
  := (NoneE s) (at level 60) : monad_scope.

Definition keep {Y : Type} (x : unit) (y : Y) : Y :=
  let hello := x in y.
Notation " x #; y " := (keep x y) (at level 60) : monad_scope.
