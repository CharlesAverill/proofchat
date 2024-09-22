(** Server
    
    In this section, we define program logic for the host server
*)

Require Import Messages.
Require Import Monads.
Require Import Recdef.
Open Scope monad_scope.

(**
    The internal loop of our client sending thread
*)
Definition server_send_thread_internal (_ : unit) : optionE unit :=
    repeat_until_timeout max_int (fun _ => SomeE tt).

(**
    Wraps up all client logic: port binding, threading, etc.
*)
Definition server (host : string) (portno : port) : optionE unit :=
    (* Create a TCP socket *)
    let socket_fd := socket PF_INET SOCK_STREAM 0 in
    let socket_addr := ADDR_INET (inet_addr_of_string host) portno in
    let* _ <= print_endline ("Binding socket " ++ 
        (string_of_socket_addr socket_addr) ++ " as " ++
        (string_of_socket_addr (getsockname socket_fd))) #;
    let* _ <= bind socket_fd socket_addr #;
    (* Wait for and accept any incoming TCP connection requests. *)
    let* _ <= listen socket_fd 1 #;
    let* _ <= sleep 100 #;
    (* Receive ACK from server *)
    (* Display chatroom information to user output *)
    (* Wait for user input *)
    (* Wait for chat messages from server *)
    let* _ <= print_endline "Closing client connection" #;
    let* _ <= close socket_fd #;
    return tt.
