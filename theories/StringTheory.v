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
    | _ :: t => 
        let n := int_len_list t in
        if (n <? max_int)%sint63 then
            1 + n
        else
            max_int
    end.

Theorem int_len_list_nat : forall (X : Type) (l : list X),
    (0 <=? int_len_list l = true)%sint63.
Proof.
    intros. induction l.
    - reflexivity.
    - simpl.
      destruct (int_len_list l <? max_int)%sint63 eqn:E.
      apply leb_spec. apply leb_spec in IHl. apply ltb_spec in E.
      rewrite to_Z_add.
        etransitivity. eassumption.
        change (to_Z 1) with 1. lia.
      split.
        transitivity 0. unfold to_Z, Uint63.to_Z; simpl; lia.
        etransitivity. eassumption. 
            change (to_Z 1) with 1. lia.
        rewrite Z.add_comm. now apply Ztac.Zlt_le_add_1.
        apply leb_spec. unfold to_Z, Uint63.to_Z; simpl; lia.
Qed.

(** Get a list of [byte]s from a [string] *)
Fixpoint bytes_of_string (s : string) : bytes :=
    match s with
    | EmptyString => []
    | String a s' => (byte_of_ascii a) :: bytes_of_string s'
    end.

(** Get a [string] from a list of [byte]s *)
Fixpoint string_of_bytes (b : bytes) : string :=
    match b with
    | [] => EmptyString
    | h :: t => String (ascii_of_byte h) (string_of_bytes t)
    end.

Theorem bytes_of_string_of_bytes : forall (b : bytes),
    bytes_of_string (string_of_bytes b) = b.
Proof.
    induction b; simpl.
    - reflexivity.
    - now rewrite byte_of_ascii_of_byte, IHb.
Qed.

(** OCaml int string length *)
Definition int_len_string (s : string) : int :=
    int_len_list (bytes_of_string s).

(** Create a list of length [n] containing only [x] *)
Function create_list {X : Type} (x : X) (n : int) 
    {measure (fun x => (Z.to_nat (to_Z x))) n} : list X :=
    (if sub1_no_underflow n then
        x :: (create_list x (n - 1))
    else
        [])%sint63.
    prove_sub1.
Defined.

Definition max (x y : int) : int :=
    if (y <? x)%sint63 then x else y.

Theorem create_list_n : forall (X : Type) (x : X) (n : int),
    int_len_list (create_list X x n) = max 0 n.
Proof.
    intros. unfold create_list.
    destruct (create_list_terminate).
    destruct e. specialize (H (S x1)).
    assert (x1 < S x1)%nat by lia.
    specialize (H H0). clear H0.
    rewrite <- H with (def := create_list). simpl.
    unfold create_list_F. destruct (sub1_no_underflow n) eqn:E.
Abort.

(** Pad a string on the right with [b] until the entire string has length [target_len] *)
Definition pad_string_r (s : string) (b : byte) (target_len : int) : string :=
    s ++ (string_of_bytes (create_list byte b (target_len - int_len_string s))).

Theorem pad_string_r_len : forall (s : string) (b : byte) (n : int),
    int_len_string (pad_string_r s b n) = n.
Proof.
    induction s; intros.
    - unfold pad_string_r. simpl. cbv [int_len_string].
      simpl. rewrite sub_of_Z. change (to_Z 0) with 0.
      rewrite Z.sub_0_r, of_to_Z, bytes_of_string_of_bytes.
Abort.

(** Trim all occurances of [b] from the right side of [s] *)
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

Definition trim_null (b : bytes) : string :=
    trim_r (string_of_bytes b) "000"%byte.

(** Get the first [n] elements of [l], or fail if not enough *)
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

(** Get the last [n] elements of [l], or fail if not enough *)
Definition last_n {X : Type} (l : list X) (n : int) : optionE (list X) :=
    aux <- first_n (rev l) n ;;
    return (rev aux).

(** Split [l] into [n] lists of length [size], or fail if not enough elements *)
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
