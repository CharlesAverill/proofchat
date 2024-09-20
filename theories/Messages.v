(** * Messages

    In this section, we introduce well-formed usernames and messages, along
    with proof-carrying operations over them
*)


Require Export OCamlTypes.
Require Export StringTheory.
Require Export Monads.
Close Scope nat_scope.
Open Scope Z_scope.
Open Scope sint63_scope.


(** 
    A proof-carrying username type
*)
Record username : Type := {
      Uname : string
    ; ValidLength :
        1 <= Z.of_nat (length Uname) <= 32
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
    ((1 <=? Z.of_nat (length name)) && (Z.of_nat (length name) <=? 32))%Z
        && (no_spaces name).

(**
    Ensure that [validate_username] provides
    assurances for the properties we're
    interested in
*)
Theorem validate_username_correct :
    forall (name : string)
    (VALID : validate_username name = true),
        1 <= Z.of_nat (length name) <= 32 /\
        ~ InString space name.
Proof.
    intros. unfold validate_username in VALID.
    apply andb_true_iff in VALID.
    destruct VALID as [Length NoSpaces].
    apply andb_true_iff in Length.
    destruct Length as [NonEmpty Le32].
    apply Z.leb_le in NonEmpty, Le32.
    split.
    - now split.
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
