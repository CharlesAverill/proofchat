
type nat =
| O
| S of nat

val app : 'a1 list -> 'a1 list -> 'a1 list

val add : nat -> nat -> nat

val sub : nat -> nat -> nat

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Pos :
 sig
  val succ : positive -> positive

  val pred_double : positive -> positive

  val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1

  val to_nat : positive -> nat

  val of_succ_nat : nat -> positive
 end

module N :
 sig
  val of_nat : nat -> n
 end

val rev : 'a1 list -> 'a1 list

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

module Z :
 sig
  val double : z -> z

  val succ_double : z -> z

  val opp : z -> z

  val to_nat : z -> nat
 end

val zero : char

val one : char

val shift : bool -> char -> char

val ascii_of_pos : positive -> char

val ascii_of_N : n -> char

val ascii_of_nat : nat -> char



val get : nat -> string -> char option

val substring : nat -> nat -> string -> string

val index : nat -> string -> string -> nat option

type 'x optionE =
| SomeE of 'x
| NoneE of string

val strip_options : 'a1 optionE list -> 'a1 list optionE

val lsr0 : Uint63.t -> Uint63.t -> Uint63.t

val land0 : Uint63.t -> Uint63.t -> Uint63.t

val add0 : Uint63.t -> Uint63.t -> Uint63.t

val sub0 : Uint63.t -> Uint63.t -> Uint63.t

val mul : Uint63.t -> Uint63.t -> Uint63.t

val eqb : Uint63.t -> Uint63.t -> bool

val ltb : Uint63.t -> Uint63.t -> bool

val ltsb : Uint63.t -> Uint63.t -> bool

val lesb : Uint63.t -> Uint63.t -> bool

val size : nat

val is_zero : Uint63.t -> bool

val is_even : Uint63.t -> bool

val opp0 : Uint63.t -> Uint63.t

val to_Z_rec : nat -> Uint63.t -> z

val to_Z : Uint63.t -> z

val min_int : Uint63.t

val max_int : Uint63.t

val to_Z0 : Uint63.t -> z

type bytes = char list

val send :
  Unix.file_descr -> bytes -> Uint63.t -> Uint63.t -> Unix.msg_flag list ->
  Uint63.t

val recv :
  Unix.file_descr -> Uint63.t -> Uint63.t -> Unix.msg_flag list ->
  Uint63.t * bytes

val sub1_no_underflow : Uint63.t -> bool

type repeat_until_timeout_code =
| Recurse
| EarlyStopSuccess
| EarlyStopFailure of string

val repeat_until_timeout :
  Uint63.t -> (unit -> repeat_until_timeout_code optionE) -> unit optionE

val space : char

val space_str : string

val ampersand : char

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

val divide : 'a1 list -> Uint63.t -> Uint63.t -> 'a1 list list optionE

type username =
  string
  (* singleton inductive, whose constructor was Build_username *)

val validate_username : string -> bool

val new_username : string -> username optionE

val serialize_string : string -> bytes

val serialize_username : username -> bytes

type client_message =
| REG of username
| MESG of string
| PMSG of string * username
| EXIT of username

val serialize_client_message : client_message -> bytes

type error =
| UsernameTaken
| UsernameTooLong
| UsernameHasSpaces
| PmsgTargetNotExists
| UnknownMessageFormat
| Error

val string_of_error : error -> string

val error_of_int : Uint63.t -> error

type server_message =
| ACK of Uint63.t * username list
| MSG of username * string
| ERR of error

val resend :
  Uint63.t -> Uint63.t -> Unix.file_descr -> bytes -> Uint63.t -> unit optionE

val send_message : Unix.file_descr -> bytes -> unit optionE

val recv_message : Unix.file_descr -> Uint63.t -> bytes optionE

val recv_int : Unix.file_descr -> Uint63.t optionE

val recv_string : Unix.file_descr -> string optionE

val recv_username : Unix.file_descr -> username optionE

val recv_server_ACK : Unix.file_descr -> server_message optionE

val recv_server_MSG : Unix.file_descr -> server_message optionE

val recv_server_ERR : Unix.file_descr -> server_message optionE

val recv_server_message : Unix.file_descr -> server_message optionE

val client_send_thread : Unix.file_descr -> unit optionE

val client_recv_thread : Unix.file_descr -> unit optionE

val client : string -> int -> unit optionE
