
type 'x optionE =
| SomeE of 'x
| NoneE of string

val string_of_socket_addr : Unix.sockaddr -> string

val server : string -> int -> unit optionE
