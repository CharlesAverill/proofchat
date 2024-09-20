(** Client
    
    In this section, we define program logic for client connections
*)

Require Import Messages.
Require Import Monads.
Require Import Program.
Require Import Lia.
Require Import Recdef.
Open Scope monad_scope.

(**
    A helper lemma for below
*)
Lemma Z_lt_impl_nat_lt : forall z1 z2,
    0 <= z1 < z2 ->
    (Z.to_nat z1 < Z.to_nat z2)%nat.
Proof.
    intros. destruct H as [Z1Nonneg Z1Z2].
    assert (0 <= z2) as Z2Nonneg by lia.
    now apply Z2Nat.inj_lt.
Qed.

(**
    Defines if an OCaml integer has underflown
*)
Definition no_underflow n : bool := 
    n - 1 <? n.

(**
    The internal loop of our client sending thread
*)
Function client_send_thread_internal (timeout : int)
        {measure (fun x => (Z.to_nat (to_Z x))) timeout} : int :=
    if ((0 <=? timeout - 1) && no_underflow timeout) then
        1 + client_send_thread_internal (timeout - 1)
    else
        0.
Proof.
    intros.
    unfold no_underflow in teq.
    apply Bool.andb_true_iff in teq.
    destruct teq as [Gt0 NoUnderflow].
    apply Z_lt_impl_nat_lt. split.
    - change 0%Z with (to_Z 0).
        now apply leb_spec.
    - now apply ltb_spec.
Defined.

Definition client_send_thread (_ : unit) : optionE unit :=
    let username_string := read_line tt in
    uname <- new_username username_string ;;
    return tt.
