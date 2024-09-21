open Client

let () = print_endline "Hello, World!";
  match client "127.0.0.1" 8888 with
  | NoneE err -> failwith err
  | _ -> ()
