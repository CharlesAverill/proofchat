(** * Unix

    Replicating types from OCaml's Unix module
*)

Require Export String.
Require Export Coq.Numbers.Cyclic.Int63.Sint63.

(** ** Sockets *)

Axiom file_descr : Type.
Axiom inet_addr : Type.

Definition port : Type := int.

Inductive socket_domain : Type :=
| PF_UNIX
| PF_INET
| PF_INET6.

Inductive socket_type : Type :=
| SOCK_STREAM
| SOCK_DGRAM
| SOCK_RAW
| SOCK_SEQPACKET.

Inductive sockaddr : Type :=
| ADDR_UNIX (s : string)
| ADDR_INET (addr : inet_addr) (p : port).

Inductive msg_flag : Type :=
| MSG_OOB
| MSG_DONTROUTE
| MSG_PEEK.

(** ** Threads *)

Axiom thread : Type.
