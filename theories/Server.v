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
Axiom get_connection_list : unit -> list client_connection.

Definition server_username : username.
    assert (validate_username "SERVER" = true) by reflexivity.
    apply validate_username_correct in H.
    destruct H.
    exact {| 
        Uname := "SERVER"; 
        ValidLength := H; 
        NoSpaces := H0 |}.
Defined.

Definition server_client_communication (uname : username) (cc : client_connection) : optionE unit :=
    (* Add the new connection *)
    let* _ <= log Log_Info (uname.(Uname) ++ " has joined") #;
    let cc := {|
        cc_uname := uname; 
        cc_descr := cc.(cc_descr); 
        cc_addr := cc.(cc_addr)
    |} in
    let* _ <= add_connection cc #;
    (* Acknowledge the join *)
    _ <- send_message cc.(cc_descr) (serialize_server_message
        (ACK 
            (int_len_list (get_connection_list tt)) 
            (List.map (fun cc => cc.(cc_uname)) (get_connection_list tt))
        )) ;;
    repeat_until_timeout max_int (fun _ =>
        client_msg <- recv_client_message cc.(cc_descr) ;;
        match client_msg with
        | MESG msg =>
            let* _ <= log Log_Info (cc.(cc_uname).(Uname) ++ ": " ++ msg) #;
            _ <- SomeE (List.map (fun conn =>
                (* Don't send a message to the user that sent it *)
                if conn.(cc_uname) =? uname then
                    return tt
                else
                    send_message conn.(cc_descr) (serialize_server_message (
                        MSG uname msg
                    ))
            ) (get_connection_list tt)) ;;
            return Recurse
        | PMSG msg uname =>
            match (
            target_cc <-
                match List.filter
                    (fun cc => cc.(cc_uname) =? uname) (get_connection_list tt) with
                | [cc] => SomeE cc
                | _ => 
                    _ <- send_message cc.(cc_descr) 
                        (serialize_server_message (ERR PmsgTargetNotExists)) ;;
                    fail "PmsgTargetNotExists"
                end ;;
            _ <- send_message target_cc.(cc_descr)
                    (serialize_server_message (MSG cc.(cc_uname) ("[PM] " ++ msg))) ;;
            return log Log_Info (cc.(cc_uname).(Uname) ++ ": " ++ "[PM] " ++ msg))
            with
            | SomeE _ => return Recurse
            | NoneE s =>
                let* _ <= log Log_Error s #;
                return Recurse
            end
        | EXIT _uname =>
            let* _ <= remove_connection cc #;
            _ <- SomeE (List.map
                (fun cc => send_message cc.(cc_descr) (serialize_server_message 
                    (MSG server_username 
                    (uname.(Uname) ++ " has left"))))
                (get_connection_list tt)) ;;
            let* _ <= log Log_Info (uname.(Uname) ++ " has left") #;
            return EarlyStopSuccess
        | _ => return EarlyStopFailure ("unrecognized message") 
        end
    ).

(** Do initial processing of client connection, and then start communicating
    with the client *)
Definition init_client_comms (cc : client_connection) : optionE unit :=
    let* _ <= log Log_Debug "Accepted new connection" #;
    (* Read 33 bytes - should be a REG message *)
    match (recv_client_message cc.(cc_descr)) with
    | SomeE (REG uname) =>
        match get_connection uname with
        (* Username is not taken *)
        | None =>
            if (uname =? server_username) then
                send_message cc.(cc_descr)
                    (serialize_server_message (ERR UsernameTaken))
            else
                server_client_communication uname cc
        (* Username is taken *)
        | Some _ =>
            let* _ <= log Log_Error (uname.(Uname) ++ 
                " tried to join, but username already taken") #;
            _ <- send_message cc.(cc_descr)
                (serialize_server_message (ERR UsernameTaken)) ;;
            return tt
        end
    | NoneE s =>
        let* _ <= log Log_Error s #;
        return tt
    | _ => 
        let* _ <= log Log_Error "Unexpected message format received" #;
        send_message cc.(cc_descr)
            (serialize_server_message (ERR UnknownMessageFormat))
    end.

(**
    Accept new connections and spawn synchronization threads for each of them
*)
Definition server_accept_thread (socket_fd : file_descr) : optionE unit :=
    repeat_until_timeout max_int (fun _ =>
        let '(client_descr, client_addr) := accept socket_fd in
        let* _ <= log Log_Debug 
            ("Accepted client socket " ++ (string_of_socket_addr client_addr)) #;
        _ <- create client_connection (optionE unit) init_client_comms
            {|cc_uname := dummy_username; cc_descr := client_descr; cc_addr := client_addr|} ;;
        return Recurse
    ).

Definition server_control_thread (_ : unit) : optionE unit :=
    repeat_until_timeout max_int (fun _ => 
        command <- read_line tt ;;
        if ((command =? "exit") || (command =? "quit"))%string then
            return EarlyStopSuccess
        else
            return Recurse
    ).

(**
    Wraps up all server logic: port binding, threading, etc.
*)
Definition server (host : string) (portno : port) : optionE unit :=
    let* _ <= init_connections tt #;
    (* Create a TCP socket *)
    let socket_fd := socket PF_INET SOCK_STREAM 0 in
    let socket_addr := ADDR_INET (inet_addr_of_string host) portno in
    let* _ <= log Log_Debug ("Binding socket " ++ 
        (string_of_socket_addr socket_addr) ++ " as " ++
        (string_of_socket_addr (getsockname socket_fd))) #;
    let* _ <= bind socket_fd socket_addr #;
    (* Wait for and accept any incoming TCP connection requests. *)
    let* _ <= listen socket_fd 10 #;
    let* _ <= log Log_Info "Server started" #;
    accept_thread <- create file_descr (optionE unit) server_accept_thread socket_fd ;;
    control_thread <- create unit (optionE unit) server_control_thread tt ;;
    let* _ <= join control_thread #;
    let* _ <= log Log_Info "Closing server" #;
    let* _ <= close socket_fd #;
    return tt.
