Require Export Coq.Numbers.Cyclic.Int63.Sint63.
Open Scope sint63_scope.
Require Import Arith.
Require Export StringTheory.
Require Export Bool.

Record username : Type := {
      uname : string
    ; valid_length :
        1 <= length uname <= 32
    ; no_spaces :
        ~ InString space uname
}.

Definition validate_username (name : string) : bool :=
    let fix no_spaces (s : string) : bool :=
        match s with
        | EmptyString => true
        | String a s' =>
            if (ascii_dec a space) then
                false
            else
                no_spaces s'
        end in
    ((1 <=? length name) && (length name <=? 32))%nat
        && (no_spaces name).

Theorem validate_username_correct :
    forall (name : string)
    (VALID : validate_username name = true),
        1 <= length name <= 32 /\
        ~ InString space name.
Proof.
    intros. unfold validate_username in VALID.
    apply andb_true_iff in VALID.
    destruct VALID as [Length NoSpaces].
    apply andb_true_iff in Length.
    destruct Length as [NonEmpty Le32].
    apply Nat.leb_le in NonEmpty, Le32.
    split.
    - now split.
    - clear - NoSpaces. induction name as [| a name']. auto.
        simpl. destruct (ascii_dec a space) as [a_space|a_space].
        -- inversion NoSpaces.
        -- intro Contra. destruct Contra.
            + now apply a_space.
            + now apply (IHname' NoSpaces).
Qed.

Definition new_username (s : string) : option username.
    destruct (validate_username s) eqn:E.
    - apply validate_username_correct in E. 
      destruct E.
      exact (Some {|
        uname := s;
        valid_length := H;
        no_spaces := H0
      |}).
    - exact None.
Defined.
