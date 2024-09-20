Require Export String.
Require Export Ascii.
Open Scope string.

Fixpoint InString (a : ascii) (s : string) : Prop :=
    match s with
    | EmptyString => False
    | String a' s' =>
        (a = a') \/ InString a s'
    end.

Definition space : ascii := ascii_of_nat 32.
Goal String space EmptyString = " ".
    reflexivity.
Qed.
