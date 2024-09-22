
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val sub : Uint63.t -> Uint63.t -> Uint63.t **)

let sub = Uint63.sub

(** val ltsb : Uint63.t -> Uint63.t -> bool **)

let ltsb = Uint63.lts

(** val lesb : Uint63.t -> Uint63.t -> bool **)

let lesb = Uint63.les

(** val max_int : Uint63.t **)

let max_int =
  (Uint63.of_int (4611686018427387903))

type 'x optionE =
| SomeE of 'x
| NoneE of string

(** val sub1_no_underflow : Uint63.t -> bool **)

let sub1_no_underflow n =
  (&&) (lesb (Uint63.of_int (0)) (sub n (Uint63.of_int (1))))
    (ltsb (sub n (Uint63.of_int (1))) n)

(** val repeat_until_timeout :
    Uint63.t -> (unit -> unit optionE) -> unit optionE **)

let repeat_until_timeout x x0 =
  let rec hrec timeout f _ =
    (if sub1_no_underflow timeout
     then (fun _ ->
            (match f () with
             | SomeE x1 -> (fun _ -> SomeE x1)
             | NoneE _ ->
               (fun _ -> hrec (sub timeout (Uint63.of_int (1))) f __)) __)
     else (fun _ -> NoneE "Timeout occurred")) __
  in hrec x x0 __

(** val string_of_socket_addr : Unix.sockaddr -> string **)

let string_of_socket_addr = function
| Unix.ADDR_UNIX s' -> s'
| Unix.ADDR_INET (addr, p) ->
  (^) (Unix.string_of_inet_addr addr) ((^) ":" (string_of_int p))

(** val server_accept_thread : Unix.file_descr -> unit optionE **)

let server_accept_thread socket_fd =
  repeat_until_timeout max_int (fun _ ->
    let (client_descr, client_addr) = Unix.accept socket_fd in
    let () =
      print_endline
        ((^) "Accepted client socket "
          ((^) (string_of_socket_addr client_addr)
            ((^) " as "
              (string_of_socket_addr (Unix.getsockname client_descr)))))
    in
    NoneE "server_accept_thread recurse")

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
  let () = Unix.listen socket_fd (Uint63.of_int (10)) in
  let () = print_endline "Server started" in
  (match server_accept_thread socket_fd with
   | SomeE _ ->
     let () = print_endline "Closing server" in
     let () = Unix.close socket_fd in SomeE ()
   | NoneE err -> NoneE err)
