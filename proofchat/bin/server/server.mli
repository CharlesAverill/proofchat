
type nat =
| O
| S of nat

val app : 'a1 list -> 'a1 list -> 'a1 list

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

module Pos :
 sig
  val succ : positive -> positive

  val of_succ_nat : nat -> positive
 end

module N :
 sig
  val of_nat : nat -> n
 end

val zero : char

val one : char

val shift : bool -> char -> char

val ascii_of_pos : positive -> char

val ascii_of_N : n -> char

val ascii_of_nat : nat -> char



val rev : 'a1 list -> 'a1 list

val concat : 'a1 list list -> 'a1 list

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val add : Uint63.t -> Uint63.t -> Uint63.t

val sub : Uint63.t -> Uint63.t -> Uint63.t

val eqb : Uint63.t -> Uint63.t -> bool

val ltsb : Uint63.t -> Uint63.t -> bool

val lesb : Uint63.t -> Uint63.t -> bool

val max_int : Uint63.t

type 'x optionE =
| SomeE of 'x
| NoneE of string

type bytes = char list

val send :
  Unix.file_descr -> bytes -> Uint63.t -> Uint63.t -> Unix.msg_flag list ->
  Uint63.t

val recv :
  Unix.file_descr -> Uint63.t -> Uint63.t -> Unix.msg_flag list ->
  Uint63.t * bytes

val keep : 'a1 -> unit

val sub1_no_underflow : Uint63.t -> bool

type repeat_until_timeout_code =
| Recurse
| EarlyStopSuccess
| EarlyStopFailure of string

val repeat_until_timeout :
  Uint63.t -> (unit -> repeat_until_timeout_code) -> unit optionE

val space : char

val no_spaces : string -> bool

val string_of_socket_addr : Unix.sockaddr -> string

val int_len_list : 'a1 list -> Uint63.t

val bytes_of_string : string -> bytes

val string_of_bytes : bytes -> string

val int_len_string : string -> Uint63.t

val create_list : 'a1 -> Uint63.t -> 'a1 list

val pad_string_r : string -> char -> Uint63.t -> string

val first_n : 'a1 list -> Uint63.t -> 'a1 list optionE

val last_n : 'a1 list -> Uint63.t -> 'a1 list optionE

type username =
  string
  (* singleton inductive, whose constructor was Build_username *)

val validate_username : string -> bool

val new_username : string -> username optionE

val dummy_username : username

val serialize_string : string -> bytes

val serialize_username : username -> bytes

type client_message =
| REG of username
| MESG of string
| PMSG of string * username
| EXIT of username

val deserialize_client_message : bytes -> client_message optionE

type error =
| UsernameTaken
| UsernameTooLong
| UsernameHasSpaces
| PmsgTargetNotExists
| UnknownMessageFormat
| Error

val int_of_error : error -> Uint63.t

type server_message =
| ACK of Uint63.t * username list
| MSG of username * string
| ERR of error

val serialize_server_message : server_message -> bytes

val resend :
  Uint63.t -> Uint63.t -> Unix.file_descr -> bytes -> Uint63.t -> unit optionE

val send_message : Unix.file_descr -> bytes -> unit optionE

val recv_message : Unix.file_descr -> Uint63.t -> bytes

val cc_descr : Proofchat.Serverstate.client_connection -> Unix.file_descr

val cc_addr : Proofchat.Serverstate.client_connection -> Unix.sockaddr

val recv_client_message :
  Proofchat.Serverstate.client_connection -> unit optionE

val server_accept_thread : Unix.file_descr -> unit optionE

val server : string -> int -> unit optionE
