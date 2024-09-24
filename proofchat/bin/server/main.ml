open Server
open Proofchat.Logging

let () = print_endline "Hello, World!";
  match server "127.0.0.1" 8888 with
  | NoneE err -> fatal rc_Error err
  | _ -> ()
