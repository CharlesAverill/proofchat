
let __ = let rec f _ = Obj.repr f in Obj.repr f

type nat =
| O
| S of nat

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let app x =
  let rec app0 l m =
    match l with
    | [] -> m
    | a :: l1 -> a :: (app0 l1 m)
  in app0 x

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

module Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val of_succ_nat : nat -> positive **)

  let rec of_succ_nat = function
  | O -> XH
  | S x -> succ (of_succ_nat x)
 end

module N =
 struct
  (** val of_nat : nat -> n **)

  let of_nat = function
  | O -> N0
  | S n' -> Npos (Pos.of_succ_nat n')
 end

(** val concat : 'a1 list list -> 'a1 list **)

let concat l =
  let rec concat0 = function
  | [] -> []
  | x :: l1 -> app x (concat0 l1)
  in concat0 l

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let map f =
  let rec map0 = function
  | [] -> []
  | a :: t -> (f a) :: (map0 t)
  in map0

(** val filter : ('a1 -> bool) -> 'a1 list -> 'a1 list **)

let filter f =
  let rec filter0 = function
  | [] -> []
  | x :: l0 -> if f x then x :: (filter0 l0) else filter0 l0
  in filter0

(** val zero : char **)

let zero = '\000'

(** val one : char **)

let one = '\001'

(** val shift : bool -> char -> char **)

let shift = fun b c -> Char.chr (((Char.code c) lsl 1) land 255 + if b then 1 else 0)

(** val ascii_of_pos : positive -> char **)

let ascii_of_pos =
  let rec loop n0 p =
    match n0 with
    | O -> zero
    | S n' ->
      (match p with
       | XI p' -> shift true (loop n' p')
       | XO p' -> shift false (loop n' p')
       | XH -> one)
  in loop (S (S (S (S (S (S (S (S O))))))))

(** val ascii_of_N : n -> char **)

let ascii_of_N = function
| N0 -> zero
| Npos p -> ascii_of_pos p

(** val ascii_of_nat : nat -> char **)

let ascii_of_nat a =
  ascii_of_N (N.of_nat a)



type 'x optionE =
| SomeE of 'x
| NoneE of string

(** val add : Uint63.t -> Uint63.t -> Uint63.t **)

let add = Uint63.add

(** val sub : Uint63.t -> Uint63.t -> Uint63.t **)

let sub = Uint63.sub

(** val ltsb : Uint63.t -> Uint63.t -> bool **)

let ltsb = Uint63.lts

(** val lesb : Uint63.t -> Uint63.t -> bool **)

let lesb = Uint63.les

(** val max_int : Uint63.t **)

let max_int =
  (Uint63.of_int (4611686018427387903))

type bytes = char list

(** val send :
    Unix.file_descr -> bytes -> Uint63.t -> Uint63.t -> Unix.msg_flag list ->
    Uint63.t **)

let send = (fun a b c d e -> Unix.send a (Proofchat.Pfbytes.bytes_of_char_list b) c d e)

(** val recv :
    Unix.file_descr -> Uint63.t -> Uint63.t -> Unix.msg_flag list ->
    Uint63.t * bytes **)

let recv = Proofchat.Pfbytes.functional_read

(** val sub1_no_underflow : Uint63.t -> bool **)

let sub1_no_underflow n0 =
  (&&) (lesb (Uint63.of_int (0)) (sub n0 (Uint63.of_int (1))))
    (ltsb (sub n0 (Uint63.of_int (1))) n0)

type repeat_until_timeout_code =
| Recurse
| EarlyStopSuccess
| EarlyStopFailure of string

(** val repeat_until_timeout :
    Uint63.t -> (unit -> repeat_until_timeout_code optionE) -> unit optionE **)

let repeat_until_timeout x x0 =
  let rec hrec timeout f _ =
    (if sub1_no_underflow timeout
     then (fun _ ->
            (match f () with
             | SomeE x1 ->
               (fun _ ->
                 (match x1 with
                  | Recurse ->
                    (fun _ _ -> hrec (sub timeout (Uint63.of_int (1))) f __)
                  | EarlyStopSuccess -> (fun _ _ -> SomeE ())
                  | EarlyStopFailure s -> (fun _ _ -> NoneE s)) __ __)
             | NoneE s -> (fun _ -> NoneE s)) __)
     else (fun _ -> NoneE "Timeout occurred")) __
  in hrec x x0 __

(** val space : char **)

let space =
  ascii_of_nat (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S O))))))))))))))))))))))))))))))))

(** val no_spaces : string -> bool **)

let rec no_spaces s =
  (* If this appears, you're using String internals. Please don't *)
 (fun f0 f1 s ->
    let l = String.length s in
    if l = 0 then f0 () else f1 (String.get s 0) (String.sub s 1 (l-1)))

    (fun _ -> true)
    (fun a s' -> if (=) a space then false else no_spaces s')
    s

(** val string_of_socket_addr : Unix.sockaddr -> string **)

let string_of_socket_addr = function
| Unix.ADDR_UNIX s' -> s'
| Unix.ADDR_INET (addr, p) ->
  (^) (Unix.string_of_inet_addr addr) ((^) ":" (string_of_int p))

(** val int_len_list : 'a1 list -> Uint63.t **)

let rec int_len_list = function
| [] -> (Uint63.of_int (0))
| _ :: t ->
  let n0 = int_len_list t in
  if ltsb n0 max_int then add (Uint63.of_int (1)) n0 else max_int

(** val bytes_of_string : string -> bytes **)

let rec bytes_of_string s =
  (* If this appears, you're using String internals. Please don't *)
 (fun f0 f1 s ->
    let l = String.length s in
    if l = 0 then f0 () else f1 (String.get s 0) (String.sub s 1 (l-1)))

    (fun _ -> [])
    (fun a s' -> ((fun x -> x) a) :: (bytes_of_string s'))
    s

(** val string_of_bytes : bytes -> string **)

let rec string_of_bytes = function
| [] -> ""
| h :: t ->
  (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

    (((fun x -> x) h), (string_of_bytes t))

(** val int_len_string : string -> Uint63.t **)

let int_len_string s =
  int_len_list (bytes_of_string s)

(** val create_list : 'a1 -> Uint63.t -> 'a1 list **)

let create_list x0 x1 =
  let rec hrec _ x n0 _ =
    (if sub1_no_underflow n0
     then (fun _ -> x :: (hrec __ x (sub n0 (Uint63.of_int (1))) __))
     else (fun _ -> [])) __
  in hrec __ x0 x1 __

(** val pad_string_r : string -> char -> Uint63.t -> string **)

let pad_string_r s b target_len =
  (^) s (string_of_bytes (create_list b (sub target_len (int_len_string s))))

type username =
  string
  (* singleton inductive, whose constructor was Build_username *)

(** val validate_username : string -> bool **)

let validate_username name =
  (&&)
    ((&&) (lesb (Uint63.of_int (1)) (int_len_string name))
      (lesb (int_len_string name) (Uint63.of_int (32)))) (no_spaces name)

(** val new_username : string -> username optionE **)

let new_username s =
  let b = validate_username s in
  (if b
   then (fun _ -> SomeE s)
   else (fun _ ->
          let b0 = no_spaces s in
          if b0
          then NoneE
                 ((^) "Username length must be in range [1..32]: '"
                   ((^) s "'"))
          else NoneE ((^) "Username cannot contain spaces: '" ((^) s "'"))))
    __

(** val dummy_username : username **)

let dummy_username =
  "X"

(** val eqb : username -> username -> bool **)

let eqb =
  (=)

(** val serialize_string : string -> bytes **)

let serialize_string s =
  app (Proofchat.Pfbytes.int63_to_bytes (int_len_string s))
    (bytes_of_string s)

(** val serialize_username : username -> bytes **)

let serialize_username u =
  bytes_of_string (pad_string_r u '\x00' (Uint63.of_int (32)))

type client_message =
| REG of username
| MESG of string
| PMSG of string * username
| EXIT of username

type error =
| UsernameTaken
| UsernameTooLong
| UsernameHasSpaces
| PmsgTargetNotExists
| UnknownMessageFormat
| Error

(** val int_of_error : error -> Uint63.t **)

let int_of_error = function
| UsernameTaken -> (Uint63.of_int (0))
| UsernameTooLong -> (Uint63.of_int (1))
| UsernameHasSpaces -> (Uint63.of_int (2))
| PmsgTargetNotExists -> (Uint63.of_int (3))
| UnknownMessageFormat -> (Uint63.of_int (4))
| Error -> (Uint63.of_int (5))

type server_message =
| ACK of Uint63.t * username list
| MSG of username * string
| ERR of error

(** val serialize_server_message : server_message -> bytes **)

let serialize_server_message = function
| ACK (num_users, users) ->
  '\x00' :: (app (Proofchat.Pfbytes.int63_to_bytes num_users)
              (concat (map serialize_username users)))
| MSG (name, message) ->
  '\x01' :: (app (serialize_username name) (serialize_string message))
| ERR err -> '\x02' :: (Proofchat.Pfbytes.int63_to_bytes (int_of_error err))

(** val resend :
    Uint63.t -> Uint63.t -> Unix.file_descr -> bytes -> Uint63.t -> unit
    optionE **)

let resend x x0 x1 x2 x3 =
  let rec hrec fuel n_sent sockfd message len_msg _ =
    (if sub1_no_underflow fuel
     then (fun _ ->
            (if ltsb
                  (add n_sent
                    (send sockfd message n_sent (sub len_msg n_sent) []))
                  len_msg
             then (fun _ ->
                    hrec (sub fuel (Uint63.of_int (1)))
                      (add n_sent
                        (send sockfd message n_sent (sub len_msg n_sent) []))
                      sockfd message len_msg __)
             else (fun _ -> SomeE ())) __)
     else (fun _ -> NoneE
            ((^) "Timed out while sending message '"
              ((^) (string_of_bytes message) "'")))) __
  in hrec x x0 x1 x2 x3 __

(** val send_message : Unix.file_descr -> bytes -> unit optionE **)

let send_message sockfd message =
  resend (Uint63.of_int (100)) (Uint63.of_int (0)) sockfd message
    (int_len_list message)

(** val recv_message : Unix.file_descr -> Uint63.t -> bytes optionE **)

let recv_message sockfd len =
  let (_, out) = recv sockfd (Uint63.of_int (0)) len [] in SomeE out

(** val recv_int : Unix.file_descr -> Uint63.t optionE **)

let recv_int sockfd =
  match recv_message sockfd (Uint63.of_int (8)) with
  | SomeE n_bytes -> SomeE (Proofchat.Pfbytes.bytes_to_int63 n_bytes)
  | NoneE err -> NoneE err

(** val recv_string : Unix.file_descr -> string optionE **)

let recv_string sockfd =
  match recv_int sockfd with
  | SomeE str_len ->
    (match recv_message sockfd str_len with
     | SomeE str_bytes -> SomeE (string_of_bytes str_bytes)
     | NoneE err -> NoneE err)
  | NoneE err -> NoneE err

(** val recv_username : Unix.file_descr -> username optionE **)

let recv_username sockfd =
  match recv_message sockfd (Uint63.of_int (32)) with
  | SomeE username_bytes ->
    (match new_username (string_of_bytes username_bytes) with
     | SomeE uname -> SomeE uname
     | NoneE err -> NoneE err)
  | NoneE err -> NoneE err

(** val recv_client_REG : Unix.file_descr -> client_message optionE **)

let recv_client_REG sockfd =
  match recv_username sockfd with
  | SomeE uname -> SomeE (REG uname)
  | NoneE err -> NoneE err

(** val recv_client_MESG : Unix.file_descr -> client_message optionE **)

let recv_client_MESG sockfd =
  match recv_string sockfd with
  | SomeE msg -> SomeE (MESG msg)
  | NoneE err -> NoneE err

(** val recv_client_PMSG : Unix.file_descr -> client_message optionE **)

let recv_client_PMSG sockfd =
  match recv_username sockfd with
  | SomeE uname ->
    (match recv_string sockfd with
     | SomeE msg -> SomeE (PMSG (msg, uname))
     | NoneE err -> NoneE err)
  | NoneE err -> NoneE err

(** val recv_client_EXIT : Unix.file_descr -> client_message optionE **)

let recv_client_EXIT sockfd =
  match recv_username sockfd with
  | SomeE uname -> SomeE (EXIT uname)
  | NoneE err -> NoneE err

(** val recv_client_message : Unix.file_descr -> client_message optionE **)

let recv_client_message sockfd =
  match recv_message sockfd (Uint63.of_int (1)) with
  | SomeE code ->
    (match code with
     | [] ->
       NoneE
         ((^) "Failed to receive client message with opcode "
           (string_of_bytes code))
     | b :: l ->
       (match b with
        | '\x00' ->
          (match l with
           | [] -> recv_client_REG sockfd
           | _ :: _ ->
             NoneE
               ((^) "Failed to receive client message with opcode "
                 (string_of_bytes code)))
        | '\x01' ->
          (match l with
           | [] -> recv_client_MESG sockfd
           | _ :: _ ->
             NoneE
               ((^) "Failed to receive client message with opcode "
                 (string_of_bytes code)))
        | '\x02' ->
          (match l with
           | [] -> recv_client_PMSG sockfd
           | _ :: _ ->
             NoneE
               ((^) "Failed to receive client message with opcode "
                 (string_of_bytes code)))
        | '\x03' ->
          (match l with
           | [] -> recv_client_EXIT sockfd
           | _ :: _ ->
             NoneE
               ((^) "Failed to receive client message with opcode "
                 (string_of_bytes code)))
        | '\x04' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x05' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x06' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x07' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x08' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\t' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\n' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x0b' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x0c' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\r' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x0e' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x0f' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x10' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x11' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x12' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x13' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x14' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x15' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x16' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x17' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x18' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x19' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x1a' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x1b' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x1c' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x1d' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x1e' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x1f' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | ' ' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '!' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '"' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '#' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '$' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '%' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '&' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\'' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '(' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | ')' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '*' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '+' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | ',' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '-' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '.' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '/' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '0' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '1' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '2' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '3' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '4' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '5' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '6' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '7' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '8' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '9' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | ':' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | ';' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '<' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '=' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '>' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '?' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '@' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'A' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'B' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'C' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'D' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'E' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'F' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'G' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'H' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'I' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'J' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'K' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'L' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'M' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'N' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'O' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'P' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'Q' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'R' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'S' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'T' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'U' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'V' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'W' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'X' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'Y' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'Z' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '[' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\\' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | ']' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '^' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '_' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '`' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'a' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'b' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'c' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'd' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'e' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'f' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'g' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'h' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'i' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'j' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'k' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'l' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'm' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'n' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'o' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'p' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'q' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'r' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 's' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 't' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'u' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'v' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'w' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'x' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'y' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | 'z' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '{' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '|' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '}' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '~' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x7f' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x80' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x81' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x82' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x83' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x84' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x85' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x86' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x87' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x88' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x89' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x8a' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x8b' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x8c' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x8d' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x8e' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x8f' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x90' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x91' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x92' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x93' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x94' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x95' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x96' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x97' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x98' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x99' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x9a' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x9b' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x9c' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x9d' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x9e' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\x9f' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xa0' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xa1' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xa2' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xa3' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xa4' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xa5' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xa6' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xa7' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xa8' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xa9' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xaa' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xab' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xac' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xad' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xae' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xaf' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xb0' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xb1' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xb2' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xb3' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xb4' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xb5' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xb6' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xb7' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xb8' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xb9' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xba' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xbb' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xbc' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xbd' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xbe' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xbf' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xc0' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xc1' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xc2' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xc3' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xc4' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xc5' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xc6' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xc7' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xc8' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xc9' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xca' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xcb' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xcc' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xcd' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xce' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xcf' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xd0' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xd1' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xd2' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xd3' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xd4' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xd5' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xd6' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xd7' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xd8' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xd9' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xda' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xdb' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xdc' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xdd' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xde' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xdf' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xe0' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xe1' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xe2' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xe3' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xe4' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xe5' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xe6' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xe7' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xe8' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xe9' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xea' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xeb' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xec' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xed' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xee' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xef' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xf0' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xf1' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xf2' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xf3' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xf4' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xf5' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xf6' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xf7' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xf8' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xf9' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xfa' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xfb' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xfc' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xfd' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xfe' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))
        | '\xff' ->
          NoneE
            ((^) "Failed to receive client message with opcode "
              (string_of_bytes code))))
  | NoneE err -> NoneE err

(** val cc_uname : Proofchat.Serverstate.client_connection -> username **)

let cc_uname = function
| (cc_uname0, _, _) -> cc_uname0

(** val cc_descr :
    Proofchat.Serverstate.client_connection -> Unix.file_descr **)

let cc_descr = function
| (_, cc_descr0, _) -> cc_descr0

(** val cc_addr : Proofchat.Serverstate.client_connection -> Unix.sockaddr **)

let cc_addr = function
| (_, _, cc_addr0) -> cc_addr0

(** val server_client_communication :
    username -> Proofchat.Serverstate.client_connection -> unit optionE **)

let server_client_communication uname cc =
  let () = Proofchat.Logging._log Log_Info ((^) uname " has joined") in
  let cc0 = (uname, (cc_descr cc), (cc_addr cc)) in
  let () = Proofchat.Serverstate.add_connection cc0 in
  (match send_message (cc_descr cc0)
           (serialize_server_message (ACK
             ((int_len_list (Proofchat.Serverstate.get_connection_list ())),
             (map cc_uname (Proofchat.Serverstate.get_connection_list ()))))) with
   | SomeE _ ->
     repeat_until_timeout max_int (fun _ ->
       match recv_client_message (cc_descr cc0) with
       | SomeE client_msg ->
         (match client_msg with
          | REG _ -> SomeE (EarlyStopFailure "unrecognized message")
          | MESG msg ->
            let () =
              Proofchat.Logging._log Log_Info
                ((^) (cc_uname cc0) ((^) ": " msg))
            in
            (match SomeE
             (map (fun conn ->
               if eqb (cc_uname conn) uname
               then SomeE ()
               else send_message (cc_descr conn)
                      (serialize_server_message (MSG (uname, msg))))
               (Proofchat.Serverstate.get_connection_list ())) with
             | SomeE _ -> SomeE Recurse
             | NoneE err -> NoneE err)
          | PMSG (msg, uname0) ->
            (match match match filter (fun cc1 -> eqb (cc_uname cc1) uname0)
                                 (Proofchat.Serverstate.get_connection_list
                                   ()) with
                         | [] ->
                           (match send_message (cc_descr cc0)
                                    (serialize_server_message (ERR
                                      PmsgTargetNotExists)) with
                            | SomeE _ -> NoneE "PmsgTargetNotExists"
                            | NoneE err -> NoneE err)
                         | cc1 :: l ->
                           (match l with
                            | [] -> SomeE cc1
                            | _ :: _ ->
                              (match send_message (cc_descr cc0)
                                       (serialize_server_message (ERR
                                         PmsgTargetNotExists)) with
                               | SomeE _ -> NoneE "PmsgTargetNotExists"
                               | NoneE err -> NoneE err)) with
                   | SomeE target_cc ->
                     send_message (cc_descr target_cc)
                       (serialize_server_message (MSG ((cc_uname cc0),
                         ((^) "[PM] " msg))))
                   | NoneE err -> NoneE err with
             | SomeE _ -> SomeE Recurse
             | NoneE s ->
               let () = Proofchat.Logging._log Log_Error s in SomeE Recurse)
          | EXIT _ -> SomeE (EarlyStopFailure "unrecognized message"))
       | NoneE err -> NoneE err)
   | NoneE err -> NoneE err)

(** val init_client_comms :
    Proofchat.Serverstate.client_connection -> unit optionE **)

let init_client_comms cc =
  let () = Proofchat.Logging._log Log_Debug "Accepted new connection" in
  (match recv_client_message (cc_descr cc) with
   | SomeE x ->
     (match x with
      | REG uname ->
        (match Proofchat.Serverstate.get_connection uname with
         | Some _ ->
           let () =
             Proofchat.Logging._log Log_Error
               ((^) uname " tried to join, but username already taken")
           in
           (match send_message (cc_descr cc)
                    (serialize_server_message (ERR UsernameTaken)) with
            | SomeE _ -> SomeE ()
            | NoneE err -> NoneE err)
         | None -> server_client_communication uname cc)
      | MESG _ ->
        let () =
          Proofchat.Logging._log Log_Error
            "Unexpected message format received"
        in
        send_message (cc_descr cc)
          (serialize_server_message (ERR UnknownMessageFormat))
      | PMSG (_, _) ->
        let () =
          Proofchat.Logging._log Log_Error
            "Unexpected message format received"
        in
        send_message (cc_descr cc)
          (serialize_server_message (ERR UnknownMessageFormat))
      | EXIT _ ->
        let () =
          Proofchat.Logging._log Log_Error
            "Unexpected message format received"
        in
        send_message (cc_descr cc)
          (serialize_server_message (ERR UnknownMessageFormat)))
   | NoneE s -> let () = Proofchat.Logging._log Log_Error s in SomeE ())

(** val server_accept_thread : Unix.file_descr -> unit optionE **)

let server_accept_thread socket_fd =
  repeat_until_timeout max_int (fun _ ->
    let (client_descr, client_addr) = Unix.accept socket_fd in
    let () =
      Proofchat.Logging._log Log_Debug
        ((^) "Accepted client socket " (string_of_socket_addr client_addr))
    in
    (match (fun a b -> SomeE (Thread.create a b)) init_client_comms
             (dummy_username, client_descr, client_addr) with
     | SomeE _ -> SomeE Recurse
     | NoneE err -> NoneE err))

(** val server_control_thread : unit -> unit optionE **)

let server_control_thread _ =
  repeat_until_timeout max_int (fun _ ->
    match (fun _ -> SomeE (read_line ())) () with
    | SomeE command ->
      if (||) ((=) command "exit") ((=) command "quit")
      then SomeE EarlyStopSuccess
      else SomeE Recurse
    | NoneE err -> NoneE err)

(** val server : string -> int -> unit optionE **)

let server host portno =
  let () = Proofchat.Serverstate.init_connections () in
  let socket_fd =
    Unix.socket Unix.PF_INET Unix.SOCK_STREAM (Uint63.of_int (0))
  in
  let socket_addr = Unix.ADDR_INET ((Unix.inet_addr_of_string host), portno)
  in
  let () =
    Proofchat.Logging._log Log_Debug
      ((^) "Binding socket "
        ((^) (string_of_socket_addr socket_addr)
          ((^) " as " (string_of_socket_addr (Unix.getsockname socket_fd)))))
  in
  let () = Unix.bind socket_fd socket_addr in
  let () = Unix.listen socket_fd (Uint63.of_int (10)) in
  let () = Proofchat.Logging._log Log_Info "Server started" in
  (match (fun a b -> SomeE (Thread.create a b)) server_accept_thread socket_fd with
   | SomeE _ ->
     (match (fun a b -> SomeE (Thread.create a b)) server_control_thread () with
      | SomeE control_thread ->
        let () = Thread.join control_thread in
        let () = Proofchat.Logging._log Log_Info "Closing server" in
        let () = Unix.close socket_fd in SomeE ()
      | NoneE err -> NoneE err)
   | NoneE err -> NoneE err)
