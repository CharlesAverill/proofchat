(** * Extraction

    Where everything is extracted to OCaml
*)

Require Extraction.
Require Import StringTheory.
Require Import Client.
Require Import Server.
Require Import Monads.
Require Import Unix.
Require Import ExtrOcamlBasic.
Require Import ExtrOcamlNativeString.
Require Import ExtrOCamlInt63.

Extraction Language OCaml.
Unset Extraction Optimize.

(** ** Functions *)
Extract Inlined Constant read_line => "(fun _ -> SomeE (read_line ()))".
Extract Inlined Constant print_int => "(fun i -> print_int i; flush stdout)".
Extract Inlined Constant print_string => "(fun s -> print_string s; flush stdout)".
Extract Constant print_bytes => "(fun b -> print_bytes (Proofchat.Pfbytes.bytes_of_char_list b))".
Extract Inlined Constant print_endline => "(fun s -> print_endline s; flush stdout)".
Extract Inlined Constant string_of_int => "string_of_int".
Extract Inlined Constant socket => "Unix.socket".
Extract Inlined Constant sleep => "Unix.sleep".
Extract Inlined Constant inet_addr_of_string => "Unix.inet_addr_of_string".
Extract Inlined Constant string_of_inet_addr => "Unix.string_of_inet_addr".
Extract Inlined Constant getsockname => "Unix.getsockname".
Extract Inlined Constant inet_addr => "Unix.inet_addr".
Extract Inlined Constant file_descr => "Unix.file_descr".
Extract Inlined Constant connect => "Unix.connect".
Extract Inlined Constant accept => "Unix.accept".
Extract Inlined Constant bind => "Unix.bind".
Extract Inlined Constant listen => "Unix.listen".
Extract Constant send => "(fun a b c d e -> Unix.send a (Proofchat.Pfbytes.bytes_of_char_list b) c d e)".
Extract Constant recv => "Proofchat.Pfbytes.functional_read".
Extract Inlined Constant close => "Unix.close".
Extract Inlined Constant int63_to_bytes => "Proofchat.Pfbytes.int63_to_bytes".
Extract Inlined Constant bytes_to_int63 => "Proofchat.Pfbytes.bytes_to_int63".
Extract Inlined Constant create => "(fun a b -> SomeE (Thread.create a b))".
Extract Inlined Constant join => "Thread.join".
Extract Inlined Constant exit => "Thread.exit".
Extract Constant keep => "(fun _ -> ())".
Extract Inlined Constant log => "Proofchat.Logging._log".
Extract Inlined Constant newline => "Proofchat.Logging.newline".

(** ** Server state *)
Extract Inlined Constant init_connections => "Proofchat.Serverstate.init_connections".
Extract Inlined Constant add_connection => "Proofchat.Serverstate.add_connection".
Extract Inlined Constant remove_connection => "Proofchat.Serverstate.remove_connection".
Extract Inlined Constant get_connection => "Proofchat.Serverstate.get_connection".
Extract Inlined Constant get_connection_list => "Proofchat.Serverstate.get_connection_list".

(** ** Types *)
Extract Inlined Constant port => "int".
Extract Inductive socket_domain => 
    "Unix.socket_domain" [ "Unix.PF_UNIX" "Unix.PF_INET" "Unix.PF_INET6" ].
Extract Inductive socket_type =>
    "Unix.socket_type" [ "Unix.SOCK_STREAM" "Unix.SOCK_DGRAM" "Unix.SOCK_RAW" "Unix.SOCK_SEQPACKET" ].
Extract Inductive sockaddr =>
    "Unix.sockaddr" [ "Unix.ADDR_UNIX" "Unix.ADDR_INET" ].
Extract Inductive msg_flag =>
    "Unix.msg_flag" [ "Unix.MSG_OOB" "Unix.MSG_DONTROUTE" "Unix.MSG_PEEK" ].
Extract Inlined Constant inet_addr => "Unix.inet_addr".
Extract Inlined Constant file_descr => "Unix.file_descr".
Extract Inlined Constant thread => "Thread.t".
Extract Inductive client_connection =>
    "Proofchat.Serverstate.client_connection" 
    [ "" ].
Extract Inductive log_type =>
    "Proofchat.Logging.log_type"
    [ "Log_None" "Log_Debug" "Log_Info" 
      "Log_Warning" "Log_Error" "Log_Critical" ].

(** ** Extraction *)
Set Extraction Output Directory "proofchat/bin/client".
Recursive Extraction client.
Extraction "client.ml" client.

Set Extraction Output Directory "proofchat/bin/server".
Recursive Extraction server.
Extraction "server.ml" server.
