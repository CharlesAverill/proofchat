(** * Extraction

    Where everything is extracted to OCaml
*)

Require Extraction.
Require Import StringTheory.
Require Import Client.
Require Import Monads.
Require Import Sockets.
Require Import ExtrOcamlBasic.
Require Import ExtrOcamlNativeString.
Require Import ExtrOCamlInt63.

Extraction Language OCaml.
Set Extraction Output Directory "proofchat/bin".
Unset Extraction Optimize.

Extract Inlined Constant read_line => "read_line".
Extract Inlined Constant print_int => "print_int".
Extract Inlined Constant print_string => "print_string".
Extract Inlined Constant print_endline => "print_endline".
Extract Inlined Constant string_of_int => "string_of_int".
Extract Inlined Constant socket => "Unix.socket".
Extract Inlined Constant sleep => "Unix.sleep".
Extract Inlined Constant inet_addr_of_string => "Unix.inet_addr_of_string".
Extract Inlined Constant string_of_inet_addr => "Unix.string_of_inet_addr".
Extract Inlined Constant getsockname => "Unix.getsockname".
Extract Inlined Constant inet_addr => "Unix.inet_addr".
Extract Inlined Constant file_descr => "Unix.file_descr".
Extract Inlined Constant connect => "Unix.connect".
Extract Constant send => "(fun a b c d e -> Unix.send a (Pfbytes.bytes_of_byte_list b) c d e)".
Extract Inlined Constant close => "Unix.close".
Extract Inlined Constant int63_to_bytes => "Pfbytes.int63_to_bytes".
Extract Inlined Constant bytes_to_int63 => "Pfbytes.bytes_to_int63".

(* Extract Inlined Constant int_len_list => "Bytes.length".
Extract Inlined Constant bytes_of_string => "Bytes.of_string". *)

Extract Inlined Constant port => "int".
Extract Inductive socket_domain => 
    "Unix.socket_domain" [ "Unix.PF_UNIX" "Unix.PF_INET" "Unix.PF_INET6" ].
Extract Inductive socket_type =>
    "Unix.socket_type" [ "Unix.SOCK_STREAM" "Unix.SOCK_DGRAM" "Unix.SOCK_RAW" "Unix.SOCK_SEQPACKET" ].
Extract Inductive sockaddr =>
    "Unix.sockaddr" [ "Unix.ADDR_UNIX" "Unix.ADDR_INET" ].
Extract Inductive msg_flag =>
    "Unix.msg_flag" [ "Unix.MSG_OOB" "Unix.MSG_DONTROUTE" "Unix.MSG_PEEK" ].

Recursive Extraction client.
Extraction "client.ml" client.
