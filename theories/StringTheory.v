(** * String Theory

    A collection of [string]-related definitions
*)

Require Export String.
Require Export Ascii.
Open Scope string.

(**
    Propose whether a [string] contains a given [ascii] character
*)
Fixpoint InString (a : ascii) (s : string) : Prop :=
    match s with
    | EmptyString => False
    | String a' s' =>
        (a = a') \/ InString a s'
    end.

(**
    Standard ASCII representation of the space character
*)
Definition space : ascii := ascii_of_nat 32.

(**
    Ensure the character index is correct
*)
Goal String space EmptyString = " ".
    reflexivity.
Qed.
