(** * Monads 

    This section introduces monadic notation and a custom [option] type
    to facilitate error propagation and prevent I/O functions from being
    removed during extraction
*)

Require Import String.

(** An option type with an error message argument for the failure case,
    similar to a result *)
Inductive result (X:Type) : Type :=
  | Ok (x : X)
  | Error (s : string).
Arguments Ok {X}.
Arguments Error {X}.

Declare Scope monad_scope.
Open Scope monad_scope.

Notation " x <- e1 ;; e2" := (match e1 with
                              | Ok x => e2
                              | Error err => Error err
                              end)
         (right associativity, at level 60) : monad_scope.
Notation " let* '_' <= e1 #; e2 " := (match e1 with
                                    | tt => e2 
                                    end)
         (right associativity, at level 60) : monad_scope.
Notation " 'return' e "
  := (Ok e) (at level 60) : monad_scope.
Notation " 'fail' s "
  := (Error s) (at level 60) : monad_scope.

(** Attempt to convert a list of [result]s to a list of the [Ok] contents,
    fails on the first [Error] *)
Fixpoint strip_options {X : Type} (l : list (result X)) : result (list X) :=
  match l with
  | nil => return nil
  | cons (Ok a) t =>
    t' <- strip_options t ;;
    return (cons a t')
  | cons (Error s) _ => fail ("strip_options fail: " ++ s)
  end.
