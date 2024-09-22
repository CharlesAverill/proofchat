
val sub : Uint63.t -> Uint63.t -> Uint63.t

val ltsb : Uint63.t -> Uint63.t -> bool

val lesb : Uint63.t -> Uint63.t -> bool

val max_int : Uint63.t

type 'x optionE =
| SomeE of 'x
| NoneE of string

val sub1_no_underflow : Uint63.t -> bool

val repeat_until_timeout : Uint63.t -> (unit -> unit optionE) -> unit optionE

val string_of_socket_addr : Unix.sockaddr -> string

val server_accept_thread : Unix.file_descr -> unit optionE

val server : string -> int -> unit optionE
