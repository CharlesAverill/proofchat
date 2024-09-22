open Server

let () = print_endline "Hello, World!";
  match server "127.0.0.1" 8888 with
  | NoneE err -> failwith err
  | _ -> ()
