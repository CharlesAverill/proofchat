(** * Server
    
    In this section, we define program logic for the host server
*)

Require Import Messages.
Require Import Monads.
Require Import Recdef.
Open Scope monad_scope.

(**
    Accept new connections and spawn synchronization threads for each of them
*)
Definition server_accept_thread (socket_fd : file_descr) : optionE unit :=
    repeat_until_timeout max_int (fun _ =>
        let '(client_descr, client_addr) := accept socket_fd in
        let* _ <= print_endline 
            ("Accepted client socket " ++ (string_of_socket_addr client_addr)
             ++ " as " ++ (string_of_socket_addr (getsockname client_descr))) #;
        NoneE "server_accept_thread recurse"
    ).

(**
    Wraps up all server logic: port binding, threading, etc.
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
    let* _ <= listen socket_fd 10 #;
    let* _ <= print_endline "Server started" #;
    _ <- server_accept_thread socket_fd ;;
    (* Receive ACK from server *)
    (* Display chatroom information to user output *)
    (* Wait for user input *)
    (* Wait for chat messages from server *)
    let* _ <= print_endline "Closing server" #;
    let* _ <= close socket_fd #;
    return tt.
