open Unix

type client_connection = string * file_descr * sockaddr 
(* {
    cc_uname : string
  ; cc_descr : file_descr
  ; cc_addr : sockaddr
} *)

let atom (f : unit -> 'a) : 'a =
  let m = Mutex.create () in
  Mutex.lock m;
  try
    f ()
  with e ->
    Mutex.unlock m;
    raise e

let connections : (client_connection list) ref = 
  ref []

let init_connections () =
  atom (fun _ -> connections := [])

let add_connection (c : client_connection) =
  atom (fun _ -> connections := (c :: !connections))

let remove_connection (c : client_connection) =
  atom (fun _ -> 
    connections := List.filter (fun c' -> c' != c) !connections)

let get_connection (s : string) : client_connection option =
  atom (fun _ -> 
    match (List.filter 
      (function (cc_uname, _, _) -> cc_uname = s) !connections) with
    | h :: _ -> Some h
    | _ -> None)
