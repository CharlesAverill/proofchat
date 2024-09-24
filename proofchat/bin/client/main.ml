open Client
open Proofchat.Logging

let () = print_endline "Hello, World!";
  match client "127.0.0.1" 8888 with
  | NoneE err -> fatal rc_Error err
  | _ -> ()
