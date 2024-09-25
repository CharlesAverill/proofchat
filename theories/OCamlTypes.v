(** * OCaml Types

    A collection of extraction facilities to streamline extraction to OCaml.
    Also includes references to OCaml functions that deal with things like
    user input, networking, and operating system behavior.
*)

Require Import Recdef.
Require Import Monads.
Open Scope monad_scope.

(** Strings *)
Require Export Ascii String Strings.Byte.
Definition bytes := list byte.

(** OCaml Integers *)
Require Export Coq.Numbers.Cyclic.Int63.Sint63.
Require Export ZArith.

(** Booleans *)
Require Export Bool.

(** Miscellaneous *)
Require Export Unix.
Require Export Monads.

(** Functions *)
Axiom read_line : unit -> optionE string.
Axiom print_string : string -> unit.
Axiom print_bytes : bytes -> unit.
Axiom print_int : int -> unit.
Axiom print_endline : string -> unit.
Axiom socket : socket_domain -> socket_type -> int -> file_descr.
Axiom connect : file_descr -> sockaddr -> unit.
Axiom bind : file_descr -> sockaddr -> unit.
Axiom listen : file_descr -> int -> unit.
Axiom send : file_descr -> bytes -> int -> int -> list msg_flag -> int.
Axiom recv : file_descr -> int -> int -> list msg_flag -> int * bytes.
Axiom accept : file_descr -> file_descr * sockaddr.
Axiom close : file_descr -> unit.
Axiom string_of_int : int -> string.
Axiom sleep : int -> unit.
Axiom inet_addr_of_string : string -> inet_addr.
Axiom string_of_inet_addr : inet_addr -> string.
Axiom getsockname : file_descr -> sockaddr.
Axiom create : forall (X Y : Type), (X -> Y) -> X -> optionE thread.
Axiom join : thread -> unit.
Axiom exit : unit -> unit.

Axiom keep : forall (X : Type), X -> unit.

Axiom newline : string.

(** These are implemented in Proofchat.Pfbytes, but should be moved to a Coq
    definition soon so that we can prove correctness of deserialization *)
Axiom int63_to_bytes : int -> bytes.
Axiom bytes_to_int63 : bytes -> int.


(** Logging *)
Inductive log_type : Type :=
| Log_None | Log_Debug | Log_Info
| Log_Warning | Log_Error | Log_Critical.

Axiom log : log_type -> string -> unit.

(**
    A convenience lemma similar to [Z2Nat.inj_lt]
*)
Lemma Z_lt_impl_nat_lt : forall (z1 z2 : Z),
    (0 <= z1 < z2)%Z ->
    (Z.to_nat z1 < Z.to_nat z2)%nat.
Proof.
    intros. destruct H as [Z1Nonneg Z1Z2].
    assert (0 <= z2)%Z. {
        transitivity z1.
            assumption.
        now apply Z.lt_le_incl.
    }
    now apply Z2Nat.inj_lt.
Qed.

(**
    Defines if an OCaml integer has underflown
*)
Definition sub1_no_underflow (n : int) : bool := 
    ((0 <=? n - 1) && (n - 1 <? n))%sint63.

(**
    Prove termination for [Function]s where the recursive call is in the
    [true] branch of [if sub1_no_underflow n then ... else ...]
*)
Ltac prove_sub1 :=
    intros;
    unfold sub1_no_underflow in *;
    match goal with
    [H: _ && _ = true |- _] => 
        apply Bool.andb_true_iff in H;
        destruct H as [Gt0 NoUnderflow];
        apply Z_lt_impl_nat_lt; split; [
        change 0%Z with (to_Z 0);
            now apply leb_spec
        | now apply ltb_spec
        ]
    end.

(**
    Dictate the behavior of [repeat_until_timeout]
*)
Inductive repeat_until_timeout_code : Type :=
| Recurse | EarlyStopSuccess | EarlyStopFailure (s : string).

(** 
    Calls a function f until either it terminates with SomeE tt,
    or timeout occurs
*)
Function repeat_until_timeout (timeout : int) (f : unit -> optionE repeat_until_timeout_code)
        {measure (fun x => (Z.to_nat (to_Z x))) timeout} : optionE unit :=
    (if sub1_no_underflow timeout then
        f_tt_result <- f tt ;;
        match f_tt_result with
        | Recurse => repeat_until_timeout (timeout - 1) f
        | EarlyStopSuccess => return tt
        | EarlyStopFailure err => fail err
        end
    else
        NoneE "Timeout occurred")%sint63.
    prove_sub1.
Defined.
