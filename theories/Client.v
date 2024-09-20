(** Client
    
    In this section, we define program logic for client connections
*)

Require Import Messages.
Require Import Monads.
Require Import Program.
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
    assert (0 <= z2). {
        transitivity z1.
            assumption.
        now apply Z.lt_le_incl.
    }
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

(**
    Prints a sockaddr
*)
Definition print_socket_addr (s : sockaddr) : unit :=
    match s with
    | ADDR_UNIX s' => print_string s'
    | ADDR_INET addr p =>
        print_string (string_of_inet_addr addr) #;
        print_string ":" #;
        print_int p
    end.

(**
    Wraps up all client logic: port binding, threading, etc.
*)
Definition client (host : string) (portno : port) : optionE unit :=
    print_endline "Please enter a username (length 1-32, no spaces)" #;
    let username_string := read_line tt in
    uname <- new_username username_string ;;
    let socket_fd := socket PF_INET SOCK_STREAM (-1) in
    let socket_addr := ADDR_INET (inet_addr_of_string host) portno in
    print_string "Opening client connection to " #;
    print_socket_addr socket_addr #;
    print_string " as " #;
    print_socket_addr (getsockname socket_fd) #;
    print_endline "" #;
    return tt.
