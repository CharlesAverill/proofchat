
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



type 'x optionE =
| SomeE of 'x
| NoneE of string

val add : Uint63.t -> Uint63.t -> Uint63.t

val sub : Uint63.t -> Uint63.t -> Uint63.t

val ltsb : Uint63.t -> Uint63.t -> bool

val lesb : Uint63.t -> Uint63.t -> bool

type bytes = char list

val send :
  Unix.file_descr -> bytes -> Uint63.t -> Uint63.t -> Unix.msg_flag list ->
  Uint63.t

val sub1_no_underflow : Uint63.t -> bool

val space : char

val no_spaces : string -> bool

val string_of_socket_addr : Unix.sockaddr -> string

val int_len_list : 'a1 list -> Uint63.t

val bytes_of_string : string -> bytes

val string_of_bytes : bytes -> string

val int_len_string : string -> Uint63.t

val create_list : 'a1 -> Uint63.t -> 'a1 list

val pad_string_r : string -> char -> Uint63.t -> string

type username =
  string
  (* singleton inductive, whose constructor was Build_username *)

val validate_username : string -> bool

val new_username : string -> username optionE

type client_message =
| REG of username
| MESG of string
| PMSG of string * username
| EXIT of username

val serialize_string : string -> bytes

val serialize_username : username -> bytes

val serialize_client_message : client_message -> bytes

val resend :
  Uint63.t -> Uint63.t -> Unix.file_descr -> bytes -> Uint63.t -> unit optionE

val send_message : Unix.file_descr -> bytes -> unit optionE

val client : string -> int -> unit optionE
