(** * String Theory

    A collection of [string]-related definitions
*)

Require Export OCamlTypes.
Require Export String.
Require Export Ascii.
Require Export List.
Export ListNotations.
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

Fixpoint bytes_of_string (s : string) : bytes :=
    match s with
    | EmptyString => [x00]
    | String a s' => (byte_of_ascii a) :: bytes_of_string s'
    end.

Fixpoint string_of_bytes (b : bytes) : string :=
    match b with
    | [] | [x00] => EmptyString
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
