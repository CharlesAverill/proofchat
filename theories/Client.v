(** * Client
    
    In this section, we define program logic for client connections
*)

Require Import Messages.
Require Import Monads.
Require Import Recdef.
Open Scope monad_scope.

(**
    The internal loop of our client sending thread
*)
Definition client_send_thread_internal (_ : unit) : optionE unit :=
    repeat_until_timeout max_int (fun _ => EarlyStopFailure "not implemented").

(**
    Wraps up all client logic: port binding, threading, etc.
*)
Definition client (host : string) (portno : port) : optionE unit :=
    (* Obtain username from user *)
    let* _ <= print_endline "Please enter a username (length 1-32, no spaces)" #;
    let username_string := read_line tt in
    uname <- new_username username_string ;;
    (* Create a TCP socket to server *)
    let socket_fd := socket PF_INET SOCK_STREAM 0 in
    let socket_addr := ADDR_INET (inet_addr_of_string host) portno in
    let* _ <= print_endline ("Opening client connection to " ++ 
        (string_of_socket_addr socket_addr) ++ " as " ++
        (string_of_socket_addr (getsockname socket_fd))) #;
    let* _ <= connect socket_fd socket_addr #;
    (* Send REG message to server *)
    _ <- send_message socket_fd (serialize_client_message (REG uname)) ;;
    (* Receive ACK from server *)
    (* Display chatroom information to user output *)
    (* Wait for user input *)
    (* Wait for chat messages from server *)
    let* _ <= print_endline "Closing client connection" #;
    let* _ <= close socket_fd #;
    return tt.
