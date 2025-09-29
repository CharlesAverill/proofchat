(** * Messages

    In this section we handle serialization and deserialization of messages
    between the client and server
*)


Require Export OCamlTypes.
Require Export StringTheory.
Require Import List.
Import ListNotations.
Require Export Monads.
Require Import Recdef.
Require Import Lia.
Close Scope nat_scope.
Open Scope Z_scope.
Open Scope sint63_scope.
Open Scope list_scope.
Open Scope monad_scope.
(** ** Usernames 

    Usernames have specific properties per the project requirements. This
    section implements a dependently-typed Record type for usernames and some
    automation to create them at will.

*)

(** 
    A proof-carrying username type
*)
Record username : Type := {
      Uname : string
    ; ValidLength :
        (1 <= to_Z (int_len_string Uname) <= 32)%Z
    ; NoSpaces :
        ~ InString space Uname
    ; NoNulls :
        ~ InString (ascii_of_byte "000"%byte) Uname
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
        && (no_spaces name) && (no_nulls name).

(**
    Ensure that [validate_username] provides
    assurances for the properties we're
    interested in
*)
Theorem validate_username_correct :
    forall (name : string)
    (VALID : validate_username name = true),
        (1 <= to_Z (int_len_string name) <= 32)%Z /\
        ~ InString space name /\ ~ InString (ascii_of_byte "000"%byte) name.
Proof.
    intros. unfold validate_username in VALID.
    rewrite andb_true_iff in VALID.
      destruct VALID as (VALID & NoNulls).
    rewrite andb_true_iff in VALID.
      destruct VALID as (VALID & NoSpaces).
    rewrite andb_true_iff in VALID.
      destruct VALID as (NonEmpty & Le32).
    apply leb_spec in NonEmpty, Le32.
    split.
      change 1%Z with (to_Z 1). now split.
    split.
    - clear - NoSpaces. induction name as [| a name']. auto.
        simpl. simpl in NoSpaces. 
        destruct (ascii_dec a space) as [a_space|a_space].
        -- inversion NoSpaces.
        -- intro Contra. destruct Contra.
            + now apply a_space.
            + now apply IHname'.
    - clear - NoNulls. induction name as [| a name']. auto.
        simpl. simpl in NoNulls.
        destruct (ascii_dec a (ascii_of_byte "000"%byte)) as [a_null|a_null].
        -- inversion NoNulls.
        -- intro Contra. destruct Contra.
            + now apply a_null.
            + now apply IHname'.
Qed.

(**
    Instantiate a username object if it
    passes [validate_username], or return
    [None]
*)
Definition new_username (s : string) : result username.
    destruct (validate_username s) eqn:E.
    - apply validate_username_correct in E. 
      destruct E, H0.
      exact (Ok {|
        Uname := s;
        ValidLength := H;
        NoSpaces := H0;
        NoNulls := H1
      |}).
    - destruct (no_spaces s).
        -- (* No Spaces *)
        destruct (no_nulls s).
          exact (Error ("Username cannot contain nulls")).
        exact (Error 
            ("Username length must be in range [1..32]: '" ++ s ++ "'")).
        -- (* Spaces *)
        exact (Error ("Username cannot contain spaces: '" ++ s ++ "'")).
Defined.

(** A placeholder username *)
Definition dummy_username : username.
    assert (1 <= to_Z (int_len_string "X") <= 32)%Z by
        (split; unfold to_Z, Uint63.to_Z; simpl; lia).
    assert (~ InString space "X") by
        (intro; destruct H0; [discriminate | auto]).
    assert (~ InString (ascii_of_byte "000"%byte) "X") by
        (intro; destruct H1; [discriminate | auto]).
    exact {|Uname := "X"; ValidLength := H; NoSpaces := H0; NoNulls := H1|}.
Defined.

(** Username equality *)
Definition eqb (u1 u2 : username) : bool :=
    String.eqb u1.(Uname) u2.(Uname).

Notation "u1 =? u2" := (eqb u1 u2).

(** ** Serialization

    Here we start handling serialization and deserialization of messages
*)

(** Get a list of bytes from a string - not null-terminated *)
Definition serialize_string (s : string) : bytes :=
    (int63_to_bytes (int_len_string s)) ++ (bytes_of_string s).

(** Get a list of bytes from a username - padded to length 32 *)
Definition serialize_username (u : username) : bytes :=
    bytes_of_string (pad_string_r u.(Uname) x00 32).

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

(** Convert a [client_message] into a list of [byte]s, prepended with 
    codes representing the message type
*)
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

(** Convert a list of [byte]s into a [client_message] *)
Definition deserialize_client_message (b : bytes) : result client_message :=
    match b with
    | x00 :: t => 
        uname <- new_username (trim_null t) ;;
        return (REG uname)
    | x01 :: t => return (MESG (trim_null t))
    | x02 :: t =>
        name_bytes <- first_n t 32 ;;
        uname <- new_username (string_of_bytes name_bytes) ;;
        msg_bytes <- last_n t (int_len_list t - 32) ;;
        return (PMSG (trim_null msg_bytes) uname)
    | x03 :: t => 
        uname <- new_username (trim_null t) ;;
        return (EXIT uname)
    | _ => Error ("Client message code not recognized: " ++ (string_of_bytes b))
    end.

Lemma create_null_list_forall : forall n,
  Forall (fun b0 : byte => b0 = "000"%byte) (create_list_nat "000"%byte n).
Proof.
  induction n.
    apply Forall_nil.
  apply Forall_cons. reflexivity.
  apply IHn.
Qed.

Lemma trim_null_app : forall n l,
  trim_null (l ++ create_list_nat "000"%byte n) = trim_null l.
Proof.
  intros. pose proof (create_null_list_forall n).
  remember (create_list_nat _ _).
  clear Heql0.
  induction l.
Admitted.

Lemma trim_null_pad_r : forall (s : string) (p : int),
  trim_null (bytes_of_string (pad_string_r s "000" p)) = trim_null (bytes_of_string s).
Proof.
  intros. unfold pad_string_r. rewrite bytes_of_string_app, bytes_of_string_of_bytes.
  unfold create_list. apply trim_null_app.
Qed.

Lemma not_instring_inv : forall c a s,
  ~ InString c (String a s) -> c <> a /\ ~ InString c s.
Proof.
  intros. split.
    intro. apply H. subst. unfold InString. now left.
  intro. apply H. unfold InString. right. apply H0.
Qed.

Lemma trim_null_uname : forall uname,
  trim_null (bytes_of_string (Uname uname)) = Uname uname.
Proof.
  intros. pose proof (NoNulls uname). induction (Uname uname).
    reflexivity.
  apply not_instring_inv in H. destruct H.
Admitted.

Lemma new_username_clean : forall (name : username),
  new_username (trim_null (bytes_of_string (pad_string_r (Uname name) "000" 32))) = Ok name.
Proof.
  intros. rewrite trim_null_pad_r, trim_null_uname.
  enough (validate_username (Uname name) = true).
Admitted.

Theorem serialize_client_message_safe :
  forall (cm : client_message),
    deserialize_client_message (serialize_client_message cm) = Ok cm.
Proof.
  intros. destruct cm.
  - simpl. unfold serialize_username. now rewrite new_username_clean.
Admitted.

Theorem serialize_username_len : forall (u : username),
    int_len_list (serialize_username u) = 32.
Proof.
    intros. unfold serialize_username.
Abort.

Theorem serialize_REG_len : forall (u : username),
    int_len_list (serialize_client_message (REG u)) = 33.
Proof.
    intros. simpl.
Abort.

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

Definition string_of_error (e : error) : string :=
    match e with
    | UsernameTaken => "This username is already taken"
    | UsernameTooLong => "Username length must be in range [1..32]"
    | UsernameHasSpaces => "Username cannot contain spaces"
    | PmsgTargetNotExists => "The target of your private message does not exist"
    | UnknownMessageFormat => "Received an unknown message type"
    | Error => "Unknown error occurred"
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

(** Convert a [server_message] into a list of [byte]s, prepended with 
    codes representing the message type *)
Definition serialize_server_message (sm : server_message) : bytes :=
    match sm with
    | ACK num_users users =>
        x00 :: (int63_to_bytes num_users) ++ (List.concat (List.map serialize_username users))
    | MSG name message =>
        x01 :: serialize_username name ++ serialize_string message
    | ERR err =>
        x02 :: int63_to_bytes (int_of_error err)
    end.

(** Convert a list of [byte]s into a [server_message] *)
Definition deserialize_server_message (b : bytes) : result server_message :=
    match b with
    (** This case isn't helpful... why should we deserialize 
        something when we don't even know how many bytes to pull?
        Below, [recv_ACK] does basically the same thing.
        But also, we can just always receive some large number of
        bytes and assume that we have enough space. *)
    | x00 :: t =>
        num_users_bytes <- first_n t 8 ;;
        users_bytes <- last_n t (int_len_list t - 8) ;;
        let num_users := bytes_to_int63 num_users_bytes in
        usernames_bytes <- divide byte t 32 num_users ;;
        let option_usernames := List.map 
            (fun b => new_username (string_of_bytes b)) usernames_bytes in
        usernames <- strip_options option_usernames ;;
        return (ACK num_users usernames)
    | x01 :: t =>
        username_bytes <- first_n t 32 ;;
        uname <- new_username (string_of_bytes username_bytes) ;;
        t <- last_n t (int_len_list t - 32) ;;
        len_msg_bytes <- first_n t 8 ;;
        let len_msg := bytes_to_int63 len_msg_bytes in
        msg_bytes <- last_n t len_msg ;;
        return (MSG uname (string_of_bytes msg_bytes))
    | x02 :: t =>
        err_bytes <- first_n t 8 ;;
        return (ERR (error_of_int (bytes_to_int63 err_bytes)))
    | _ => fail ("Failed to deserialize bytes: " ++ (string_of_bytes b))
    end.

(** Sends a message to a socket, checking for the case where only some of the
    message was transmitted. Will attempt to send the remaining bytes (recursively)
    [fuel] times *)
Function resend 
        (fuel : int) (n_sent : int)
        (sockfd : file_descr) (message : bytes)
        (len_msg : int)
        {measure (fun x => (Z.to_nat (to_Z x))) fuel}
        : result unit :=
    if sub1_no_underflow fuel then
        let send_result := n_sent + send sockfd message n_sent (len_msg - n_sent) [] in
        (* If uncommented, the extractor will call [send] twice, because it tries
           to unfold [send_result] and put its rvalue in place of all occurrances *)
        (* let* _ <= print_endline ("Sent " ++ (string_of_int send_result) ++ " bytes") #; *)
        if send_result <? len_msg then
            resend (fuel - 1) send_result sockfd message len_msg
        else
            Ok tt
    else
        Error ("Timed out while sending message '" ++ (string_of_bytes message) ++ "'").
    prove_sub1.
Defined.

(** Wrapper for [resend] *)
Definition send_message (sockfd : file_descr) (message : bytes) : result unit :=
    resend 100 0 sockfd message (int_len_list message).

(** Receives a message from a socket *)
Definition recv_message (sockfd : file_descr) (len : int) : result bytes :=
    match recv sockfd 0 len [] with
    | (_, out) => Ok out
    end.

(** Receives a message from a socket and deserializes it as a client message *)
(* Definition recv_client_message (sockfd : file_descr) : result client_message :=
    msg_bytes <- recv_message sockfd max_message_len ;;
    deserialize_client_message msg_bytes. *)

(** Receives an int from a socket *)
Definition recv_int (sockfd : file_descr) : result int :=
    n_bytes <- recv_message sockfd 8 ;;
    return bytes_to_int63 (n_bytes).

(** Receives a string from a socket *)
Definition recv_string (sockfd : file_descr) : result string :=
    str_len <- recv_int sockfd ;;
    str_bytes <- recv_message sockfd str_len ;;
    return string_of_bytes (str_bytes).

Definition recv_username (sockfd : file_descr) : result username :=
    username_bytes <- recv_message sockfd 32 ;;
    uname <- new_username (string_of_bytes username_bytes) ;;
    return uname.

(** Receives an ACK message *)
Definition recv_server_ACK (sockfd : file_descr) : result server_message :=
    (* Receive a serialized int63 detailing number of connected users *)
    num_users <- recv_int sockfd ;;
    usernames_bytes <- recv_message sockfd (num_users * 32) ;;
    usernames_split <- divide byte usernames_bytes 32 num_users ;;
    let option_usernames := List.map
        (fun b => new_username (string_of_bytes b)) usernames_split in
    usernames <- strip_options option_usernames ;;
    return ACK num_users usernames.

(** Receives a server MSG message *)
Definition recv_server_MSG (sockfd : file_descr) : result server_message :=
    username <- recv_username sockfd ;;
    msg <- recv_string sockfd ;;
    return MSG username msg.

Definition recv_server_ERR (sockfd : file_descr) : result server_message :=
    err_code <- recv_int sockfd ;;
    return ERR (error_of_int err_code).

(** Consumes the message code and dispatches to specific receiver functions to
    deserialize and parse a server message*)
Definition recv_server_message (sockfd : file_descr) : result server_message :=
    code <- recv_message sockfd 1 ;;
    match code with
    | [x00] => recv_server_ACK sockfd
    | [x01] => recv_server_MSG sockfd
    | [x02] => recv_server_ERR sockfd
    | _ => fail ("Failed to receieve server message with opcode " ++ (string_of_bytes code))
    end.

Definition recv_client_REG (sockfd : file_descr) : result client_message :=
    uname <- recv_username sockfd ;;
    return REG uname.

Definition recv_client_MESG (sockfd : file_descr) : result client_message :=
    msg <- recv_string sockfd ;;
    return MESG msg.

Definition recv_client_PMSG (sockfd : file_descr) : result client_message :=
    uname <- recv_username sockfd ;;
    msg <- recv_string sockfd ;;
    return PMSG msg uname.

Definition recv_client_EXIT (sockfd : file_descr) : result client_message :=
    uname <- recv_username sockfd ;;
    return EXIT uname.

Definition recv_client_message (sockfd : file_descr) : result client_message :=
    code <- recv_message sockfd 1 ;;
    match code with
    | [x00] => recv_client_REG sockfd
    | [x01] => recv_client_MESG sockfd
    | [x02] => recv_client_PMSG sockfd
    | [x03] => recv_client_EXIT sockfd
    | _ => fail ("Failed to receive client message with opcode " ++ (string_of_bytes code))
    end.
