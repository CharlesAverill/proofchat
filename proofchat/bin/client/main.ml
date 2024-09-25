open Client
open Proofchat.Logging

let address = ref "127.0.0.1"  (* Default address *)
let port = ref 8888            (* Default port *)

(* Parse command-line arguments *)
let speclist = [
  ("-a", Arg.Set_string address, "Address to connect to (default: 127.0.0.1)");
  ("-p", Arg.Set_int port, "Port to connect to (default: 8888)");
]

let usage_msg = "Usage: dune exec -- proofchat.client [-a address] [-p port]"

let () =
  (* Parse the arguments based on the speclist *)
  Arg.parse speclist (fun _ -> ()) usage_msg;

  (* Attempt to connect using the provided or default address/port *)
  match client !address !port with
  | NoneE err -> fatal rc_Error err
  | _ -> ()
