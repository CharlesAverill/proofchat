(** * Monads 

    This section introduces monadic notation and a custom [option] type
    to facilitate error propagation and prevent I/O functions from being
    removed during extraction
*)

Require Import String.

(** An option type with an error message argument for the failure case,
    similar to a result *)
Inductive optionE (X:Type) : Type :=
  | SomeE (x : X)
  | NoneE (s : string).
Arguments SomeE {X}.
Arguments NoneE {X}.

Declare Scope monad_scope.
Open Scope monad_scope.

Notation " x <- e1 ;; e2" := (match e1 with
                              | SomeE x => e2
                              | NoneE err => NoneE err
                              end)
         (right associativity, at level 60) : monad_scope.
Notation " let* '_' <= e1 #; e2 " := (match e1 with
                                    | tt => e2 
                                    end)
         (right associativity, at level 60) : monad_scope.
Notation " 'return' e "
  := (SomeE e) (at level 60) : monad_scope.
Notation " 'fail' s "
  := (NoneE s) (at level 60) : monad_scope.

(** Attempt to convert a list of [optionE]s to a list of the [SomeE] contents,
    fails on the first [NoneE] *)
Fixpoint strip_options {X : Type} (l : list (optionE X)) : optionE (list X) :=
  match l with
  | nil => return nil
  | cons (SomeE a) t =>
    t' <- strip_options t ;;
    return (cons a t')
  | cons (NoneE s) _ => fail ("strip_options fail: " ++ s)
  end.
