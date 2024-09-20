(** OCaml Types

    A collection of extraction facilities to streamline extraction to OCaml.
    Also includes references to OCaml functions that deal with things like
    user input, networking, and operating system behavior.
*)

(** Strings *)
Require Export Ascii String.

(** OCaml Integers *)
Require Export Coq.Numbers.Cyclic.Int63.Sint63.
Require Export ZArith.

(** Booleans *)
Require Export Bool.

(** Miscellaneous *)
Require Export Sockets.

(** Functions *)
Axiom read_line : unit -> string.
Axiom print_string : string -> unit.
Axiom print_int : int -> unit.
Axiom print_endline : string -> unit.
Axiom socket : socket_domain -> socket_type -> int -> file_descr.
Axiom sleep : int -> unit.
Axiom inet_addr_of_string : string -> inet_addr.
Axiom string_of_inet_addr : inet_addr -> string.
Axiom getsockname : file_descr -> sockaddr.
