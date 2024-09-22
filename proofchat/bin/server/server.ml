
type 'x optionE =
| SomeE of 'x
| NoneE of string

(** val string_of_socket_addr : Unix.sockaddr -> string **)

let string_of_socket_addr = function
| Unix.ADDR_UNIX s' -> s'
| Unix.ADDR_INET (addr, p) ->
  (^) (Unix.string_of_inet_addr addr) ((^) ":" (string_of_int p))

(** val server : string -> int -> unit optionE **)

let server host portno =
  let socket_fd =
    Unix.socket Unix.PF_INET Unix.SOCK_STREAM (Uint63.of_int (0))
  in
  let socket_addr = Unix.ADDR_INET ((Unix.inet_addr_of_string host), portno)
  in
  let () =
    print_endline
      ((^) "Binding socket "
        ((^) (string_of_socket_addr socket_addr)
          ((^) " as " (string_of_socket_addr (Unix.getsockname socket_fd)))))
  in
  let () = Unix.bind socket_fd socket_addr in
  let () = Unix.listen socket_fd (Uint63.of_int (1)) in
  let () = Unix.sleep (Uint63.of_int (100)) in
  let () = print_endline "Closing client connection" in
  let () = Unix.close socket_fd in SomeE ()
