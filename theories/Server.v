(** * Server

    In this section, we define program logic for the host server
*)

Require Import Messages.
Require Import Monads.
Require Import Recdef.
Open Scope monad_scope.


(** ** Server state 

    In this section, we define types and axioms to bind to the
    Proofchat.Serverstate module that handles CRUD operations for the server
    in a thread-safe manner using mutexes.
*)
Record client_connection : Type := {
      cc_uname : username
    ; cc_descr : file_descr
    ; cc_addr : sockaddr
}.
Axiom init_connections : unit -> unit.
Axiom add_connection : client_connection -> unit.
Axiom remove_connection : client_connection -> unit.
Axiom get_connection : username -> option client_connection.

Definition recv_client_message (cc : client_connection) : optionE unit :=
    let* _ <= print_endline "Accepted, can receive!" #;
    (* Read 33 bytes - should be a REG message *)
    reg_bytes <- recv_message cc.(cc_descr) 33 ;;
    match (deserialize_client_message reg_bytes) with
    | SomeE (REG uname) =>
        match get_connection uname with
        (* Username is not taken *)
        | None => 
            (* Add the new connection *)
            let* _ <= print_endline (uname.(Uname) ++ " has joined") #;
            let cc := {|
                cc_uname := uname; 
                cc_descr := cc.(cc_descr); 
                cc_addr := cc.(cc_addr)
            |} in
            let* _ <= add_connection cc #;
            (* temporary *)
            (* let* _ <= remove_connection cc #; *)
            return tt
        (* Username is taken *)
        | Some _ =>
            let* _ <= print_endline (uname.(Uname) ++ 
                " tried to join, but username already taken") #;
            _ <- send_message cc.(cc_descr)
                (serialize_server_message (ERR UsernameTaken)) ;;
            return tt
        end
    | _ => 
        send_message cc.(cc_descr)
            (serialize_server_message (ERR UnknownMessageFormat))
    end.

(**
    Accept new connections and spawn synchronization threads for each of them
*)
Definition server_accept_thread (socket_fd : file_descr) : optionE unit :=
    repeat_until_timeout max_int (fun _ =>
        let '(client_descr, client_addr) := accept socket_fd in
        let* _ <= print_endline 
            ("Accepted client socket " ++ (string_of_socket_addr client_addr)) #;
        let* _ <= keep thread (create client_connection (optionE unit) recv_client_message
            {|cc_uname := dummy_username; cc_descr := client_descr; cc_addr := client_addr|}) #;
        Recurse
    ).

(**
    Wraps up all server logic: port binding, threading, etc.
*)
Definition server (host : string) (portno : port) : optionE unit :=
    let* _ <= init_connections tt #;
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
    let* _ <= print_endline "Closing server" #;
    let* _ <= close socket_fd #;
    return tt.
