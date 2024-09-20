open Client

let () = print_endline "Hello, World!";
  match client "12.7.0.0.1" 8888 with
  | NoneE err -> failwith err
  | _ -> ()
