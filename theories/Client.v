(** * Client
    
    In this section, we define program logic for client connections
*)

Require Import Messages.
Require Import Monads.
Require Import OCamlTypes.
Require Import Recdef.
Open Scope monad_scope.
Open Scope sint63_scope.

(**
    The internal loop of our client sending thread
*)
Definition client_send_thread (sockfd : file_descr) : optionE unit :=
    repeat_until_timeout max_int (fun _ => 
        let* _ <= print_string ">>> " #;
        msg <- read_line tt ;;
        let* _ <= print_endline "read line" #;
        match ((if (1 <? int_len_string msg) then
            (* Check if a private message *)
            let first_char := match get 0 msg with
                              | Some a => a
                              (* Not possible *)
                              | None => space
                              end in
            if Ascii.eqb first_char ampersand then
                (* First space splits username and message *)
                let split_idx := match index 0 space_str msg with
                                 | None => O
                                 | Some n => n
                                 end in
                let msg_text :=
                    substring (split_idx + 1) 
                        (Z.to_nat (to_Z (int_len_string msg)) - split_idx) msg in
                let uname_text :=
                    substring 1 (split_idx - 1) msg in
                uname <- new_username uname_text ;;
                send_message sockfd (serialize_client_message (PMSG msg_text uname))
            else
                send_message sockfd (serialize_client_message 
                    (MESG msg))
        else
            SomeE tt)) with
        | SomeE _ => SomeE Recurse
        | NoneE s => 
            let* _ <= log Log_Error s #;
            SomeE Recurse
        end
    ).

(**
    The internal loop of our client receiving thread
*)
Definition client_recv_thread (sockfd : file_descr) : optionE unit :=
    repeat_until_timeout max_int (fun _ => 
        server_msg <- recv_server_message sockfd ;;
        let* _ <= (match server_msg with
        | MSG uname msg =>
            let* _ <= print_endline (uname.(Uname) ++ ": " ++ msg) #;
            print_string ">>> "
        | ERR err =>
            let* _ <= print_endline "" #;
            let* _ <= log Log_Error (string_of_error err) #;
            print_string ">>> "
        | _ => tt
        end) #;
        SomeE Recurse
    ).

(**
    Wraps up all client logic: port binding, threading, etc.
*)
Definition client (host : string) (portno : port) : optionE unit :=
    (* Obtain username from user *)
    let* _ <= print_endline "Please enter a username (length 1-32, no spaces)" #;
    username_string <- read_line tt ;;
    uname <- new_username username_string ;;
    (* Create a TCP socket to server *)
    let socket_fd := socket PF_INET SOCK_STREAM 0 in
    let socket_addr := ADDR_INET (inet_addr_of_string host) portno in
    let* _ <= log Log_Debug ("Opening client connection to " ++ 
        (string_of_socket_addr socket_addr) ++ " as " ++
        (string_of_socket_addr (getsockname socket_fd))) #;
    let* _ <= connect socket_fd socket_addr #;
    (* Send REG message to server *)
    _ <- send_message socket_fd (serialize_client_message (REG uname)) ;;
    (* Receive ACK from server *)
    server_ack <- recv_server_message socket_fd ;;
    num_users <- (match server_ack with
                  | ACK num_users _ => return num_users
                  | ERR s => fail (string_of_error s)
                  | _ => fail "Server denied connection"
                  end) ;;
    let* _ <= log Log_Info "Server accepted connection" #;
    (* Display chatroom information to user output *)
    let* _ <= log Log_Info ("Total users: " ++ (string_of_int num_users)) #;
    (* Get user input *)
    input_thread <- create file_descr (optionE unit) client_send_thread socket_fd ;;
    (* Display server messages *)
    recv_thread <- create file_descr (optionE unit) client_recv_thread socket_fd ;;
    let* _ <= join input_thread #;
    (* let* _ <= join recv_thread #; *)
    let* _ <= log Log_Info "Closing connection to server" #;
    let* _ <= close socket_fd #;
    return tt.
