(** * String Theory

    A collection of [string]-related definitions
*)

Require Export OCamlTypes.
Require Export String.
Require Export Ascii.
Require Export List.
Require Import Lia.
Export ListNotations.
Open Scope string.
Open Scope Z_scope.
Close Scope nat_scope.

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

(**
    Returns [true] if [s] contains no space characters
*)
Fixpoint no_spaces (s : string) : bool :=
    match s with
    | EmptyString => true
    | String a s' =>
        if (ascii_dec a space) then
            false
        else
            no_spaces s'
    end.

(**
    Prints a sockaddr
*)
Definition string_of_socket_addr (s : sockaddr) : string :=
    match s with
    | ADDR_UNIX s' => s'
    | ADDR_INET addr p =>
        (string_of_inet_addr addr) ++ ":" ++ (string_of_int p)
    end.


(** OCaml int list length *)
Fixpoint int_len_list {X : Type} (l : list X) : int :=
    match l with
    | [] => 0
    | _ :: t => 1 + int_len_list t
    end.

(* Theorem int_len_list_nat : forall (X : Type) (l : list X),
    (0 <=? int_len_list l = true)%sint63.
Proof.
    induction l.
    - reflexivity.
    - simpl. apply leb_spec. apply leb_spec in IHl.
      destruct (int_len_list l <? max_int)%sint63 eqn:E.
      -- change (to_Z 0) with 0 in *.
         transitivity 1. lia. rewrite to_Z_add;
         change (to_Z 1) with 1. lia.
         split. transitivity 0. unfold to_Z; simpl; lia.
         lia. rewrite Z.add_comm. apply Ztac.Zlt_le_add_1.
         now apply ltb_spec.
      -- lia.
Qed. *)

Fixpoint bytes_of_string (s : string) : bytes :=
    match s with
    | EmptyString => []
    | String a s' => (byte_of_ascii a) :: bytes_of_string s'
    end.

Fixpoint string_of_bytes (b : bytes) : string :=
    match b with
    | [] => EmptyString
    | h :: t => String (ascii_of_byte h) (string_of_bytes t)
    end.

(** OCaml int string length *)
Definition int_len_string (s : string) : int :=
    int_len_list (bytes_of_string s).

Function create_list {X : Type} (x : X) (n : int) 
    {measure (fun x => (Z.to_nat (to_Z x))) n} : list X :=
    (if sub1_no_underflow n then
        x :: (create_list x (n - 1))
    else
        [])%sint63.
    prove_sub1.
Defined.

Definition pad_string_r (s : string) (b : byte) (target_len : int) : string :=
    s ++ (string_of_bytes (create_list byte b (target_len - int_len_string s + 1))). 

Definition trim_r (s : string) (b : byte) : string :=
    let fix aux (sb : bytes) : bytes :=
        match sb with
        | [] => []
        | a :: s' => 
            if (a =? b)%byte then
                aux s'
            else
                a :: s'
        end in
    string_of_bytes (rev (aux (rev (bytes_of_string s)))).

Fixpoint first_n {X : Type} (l : list X) (n : int) : optionE (list X) :=
    match l with
    | [] => if (n =? 0)%sint63 then SomeE [] else NoneE "first_n failure"
    | h :: t => 
        if (1 <=? n)%sint63 then
            rec_answer <- first_n t (n - 1) ;;
            return (h :: rec_answer)
        else
            return []
    end.

Definition last_n {X : Type} (l : list X) (n : int) : optionE (list X) :=
    aux <- first_n (rev l) n ;;
    return (rev aux).

Function divide {X : Type} (l : list X) (size n : int) 
    {measure (fun x => (Z.to_nat (to_Z x))) n}: optionE (list (list X)) :=
    if sub1_no_underflow n then
        first <- first_n l size ;;
        last <- (last_n l (int_len_list l - size)) ;;
        rec_answer <- divide last size (n - 1) ;;
        return (first :: rec_answer)
    else
        return [].
    prove_sub1.
Defined.
