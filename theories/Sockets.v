(** * Sockets

    Replicating socket behavior from OCaml's Unix module
*)

Inductive socket_domain : Type :=
| PF_UNIX
| PF_INET
| PF_INET6.

Inductive socket_type : Type :=
| SOCK_STREAM
| SOCK_DGRAM
| SOCK_RAW
| SOCK_SEQPACKET.
