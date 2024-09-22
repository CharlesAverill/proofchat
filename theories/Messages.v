(** * Messages

    In this section, we introduce well-formed usernames and messages, along
    with proof-carrying operations over them
*)


Require Export OCamlTypes.
Require Export StringTheory.
Require Import List.
Import ListNotations.
Require Export Monads.
Require Import Recdef.
Close Scope nat_scope.
Open Scope Z_scope.
Open Scope sint63_scope.
Open Scope list_scope.
Open Scope monad_scope.

(** 
    A proof-carrying username type
*)
Record username : Type := {
      Uname : string
    ; ValidLength :
        (1 <= to_Z (int_len_string Uname) <= 32)%Z
    ; NoSpaces :
        ~ InString space Uname
}.

(**
    Returns [true] if
    - The length of [name] is in 
        #<math>
            <mo>
                [</mo><mn>1</mn><mo>&#x22ef;</mo><mn>32</mn><mo>]
            </mo>
        </math>#
    - [name] contains no spaces
    and [false] otherwise
*)
Definition validate_username (name : string) : bool :=
    ((1 <=? int_len_string name) && (int_len_string name <=? 32))
        && (no_spaces name).

(**
    Ensure that [validate_username] provides
    assurances for the properties we're
    interested in
*)
Theorem validate_username_correct :
    forall (name : string)
    (VALID : validate_username name = true),
        (1 <= to_Z (int_len_string name) <= 32)%Z /\
        ~ InString space name.
Proof.
    intros. unfold validate_username in VALID.
    apply andb_true_iff in VALID.
    destruct VALID as [Length NoSpaces].
    apply andb_true_iff in Length.
    destruct Length as [NonEmpty Le32].
    apply leb_spec in NonEmpty, Le32.
    split.
    - change 1%Z with (to_Z 1). now split. 
    - clear - NoSpaces. induction name as [| a name']. auto.
        simpl. simpl in NoSpaces. 
        destruct (ascii_dec a space) as [a_space|a_space].
        -- inversion NoSpaces.
        -- intro Contra. destruct Contra.
            + now apply a_space.
            + now apply IHname'.
Qed.

(**
    Instantiate a username object if it
    passes [validate_username], or return
    [None]
*)
Definition new_username (s : string) : optionE username.
    destruct (validate_username s) eqn:E.
    - apply validate_username_correct in E. 
      destruct E.
      exact (SomeE {|
        Uname := s;
        ValidLength := H;
        NoSpaces := H0
      |}).
    - destruct (no_spaces s).
        -- (* No Spaces *) 
        exact (NoneE "Username length must be in range [1..32]").
        -- (* Spaces *)
        exact (NoneE "Username cannot contain spaces").
Defined.


(**
    The message types sent by a client to the server
*)
Inductive client_message : Type :=
(**
    Purpose: Register new user "name" with the chat server.

    Server Response: [ACK] with number of users and list
        of users. Send message to all other
        users that "name" has joined chat.
*)
| REG   (name : username)
(**
    Purpose: Send broadcast "message" to the entire chatroom.
    
    Server Response: Send "message" to all users in
        chatroom except sender. Include
        username of sender.
*)
| MESG  (message : string)
(**
    Purpose: Send private "message" to "user".
    
    Server Response: Send "message" to "user". Include
        username of sender.
*)
| PMSG  (message : string) (target : username)
(**
    Purpose: Inform that "name" is leaving chat.
    
    Server Response: De-register "name". [ACK] with
        updated number of users and list of
        remaining users. Send message to
        remaining users that "name" has
        left chat.
*)
| EXIT  (name : username).

Definition serialize_string (s : string) : bytes :=
    (int63_to_bytes (int_len_string s)) ++ (bytes_of_string s).

Definition serialize_username (u : username) : bytes :=
    bytes_of_string (pad_string_r u.(Uname) x00 32).

Definition serialize_client_message (cm : client_message) : bytes :=
    match cm with
    | REG name =>
        x00 :: serialize_username name
    | MESG msg =>
        x01 :: serialize_string msg
    | PMSG message name =>
        x02 :: serialize_username name ++ serialize_string message
    | EXIT name =>
        x03 :: serialize_username name
    end.

Definition deserialize_client_message (b : bytes) : optionE client_message :=
    match b with
    | x00 :: t => 
        uname <- new_username (string_of_bytes t) ;;
        return (REG uname)
    | x01 :: t => return (MESG (string_of_bytes t))
    | x02 :: t =>
        name_bytes <- first_n t 32 ;;
        uname <- new_username (string_of_bytes name_bytes) ;;
        msg_bytes <- last_n t (int_len_list t - 32) ;;
        return (PMSG (string_of_bytes msg_bytes) uname)
    | x03 :: t => 
        uname <- new_username (string_of_bytes t) ;;
        return (EXIT uname)
    | _ => NoneE ("Client message code not recognized: " ++ (string_of_bytes b))
    end.

(**
    Types of server errors that can be sent to clients
*)
Inductive error : Type :=
| UsernameTaken
| UsernameTooLong
| UsernameHasSpaces
| PmsgTargetNotExists
| UnknownMessageFormat
| Error.

(**
    Map errors to OCaml integers
*)
Definition int_of_error (e : error) : int :=
    match e with
    | UsernameTaken => 0
    | UsernameTooLong => 1
    | UsernameHasSpaces => 2
    | PmsgTargetNotExists => 3
    | UnknownMessageFormat => 4
    | Error => 5
    end.

(**
    Map OCaml integers to errors
*)
Definition error_of_int (n : int) : error :=
    match to_Z n with
    | 0 => UsernameTaken
    | 1 => UsernameTooLong
    | 2 => UsernameHasSpaces
    | 3 => PmsgTargetNotExists
    | 4 => UnknownMessageFormat
    | _ => Error
    end.

(**
    The message types sent from the server to clients
*)
Inductive server_message : Type :=
(**
    Purpose: Confirms successful registration,
        indicates number of users in chat
        (including user who just joined),
        lists all users (including user who
        just joined).
    Client Response: Displays information to user.
        Prompts user for chat messages.
*)
| ACK (num_users : int) (users : list username)
(**
    Purpose: Chat "message" from "username" to be
        sent to clients. If the message is to
        inform of a user joining or leaving
        chat, "name" is set to “SERVER”.
    Client Response: Displays username of sender and
        message to user.
*)
| MSG (name : username) (message : string)
(**
    Purpose: An error has occurred. Error codes:
        0: Username taken
        1: Username too long
        2: Username contains spaces
        3: Unknown user for private
        message
        4: Unknown message format
    Client Response: Handles error appropriately
        depending on errorcode.
*)
| ERR (err : error).

Definition serialize_server_message (sm : server_message) : bytes :=
    match sm with
    | ACK num_users users =>
        x00 :: (int63_to_bytes num_users) ++ (List.concat (List.map serialize_username users))
    | MSG name message =>
        x01 :: serialize_username name ++ serialize_string message
    | ERR err =>
        x02 :: int63_to_bytes (int_of_error err)
    end.

Definition deserialize_server_message (b : bytes) : optionE server_message :=
    match b with
    | x00 :: t =>
        num_users_bytes <- first_n t 8 ;;
        users_bytes <- last_n t (int_len_list t - 8) ;;
        let num_users := bytes_to_int63 num_users_bytes in
        usernames_bytes <- divide byte t 32 num_users ;;
        let option_usernames := List.map 
            (fun b => new_username (string_of_bytes b)) usernames_bytes in
        usernames <- strip_options option_usernames ;;
        return (ACK num_users usernames)
    | _ => fail "x"
    end.

Function resend 
        (fuel : int) (n_sent : int)
        (sockfd : file_descr) (message : bytes)
        (len_msg : int)
        {measure (fun x => (Z.to_nat (to_Z x))) fuel}
        : optionE unit :=
    if sub1_no_underflow fuel then
        (* let* _ <= print_endline ("Sent " ++ (string_of_int n_sent) ++ " bytes") #; *)
        let n_sent' := send sockfd message n_sent (len_msg - n_sent) [] in
        if n_sent + n_sent' <=? len_msg then
            resend (fuel - 1) (n_sent + n_sent') sockfd message len_msg
        else
            SomeE tt
    else
        NoneE ("Timed out while sending message '" ++ (string_of_bytes message) ++ "'").
    prove_sub1.
Defined.

Definition send_message 
        (sockfd : file_descr) (message : bytes) : optionE unit :=
    let len_msg := int_len_list message in
    let n_sent := send sockfd message 0 len_msg [] in
    if n_sent <=? len_msg then
        resend 100 n_sent sockfd message len_msg
    else
        SomeE tt.
