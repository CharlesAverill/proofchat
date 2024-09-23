
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

(** val rev : 'a1 list -> 'a1 list **)

let rev l =
  let rec rev0 = function
  | [] -> []
  | x :: l' -> app (rev0 l') (x :: [])
  in rev0 l

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

(** val eqb : Uint63.t -> Uint63.t -> bool **)

let eqb = Uint63.equal

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

(** val keep : 'a1 -> unit **)

let keep = (fun _ -> ())

(** val sub1_no_underflow : Uint63.t -> bool **)

let sub1_no_underflow n0 =
  (&&) (lesb (Uint63.of_int (0)) (sub n0 (Uint63.of_int (1))))
    (ltsb (sub n0 (Uint63.of_int (1))) n0)

type repeat_until_timeout_code =
| Recurse
| EarlyStopSuccess
| EarlyStopFailure of string

(** val repeat_until_timeout :
    Uint63.t -> (unit -> repeat_until_timeout_code) -> unit optionE **)

let repeat_until_timeout x x0 =
  let rec hrec timeout f _ =
    (if sub1_no_underflow timeout
     then (fun _ ->
            (match f () with
             | Recurse ->
               (fun _ -> hrec (sub timeout (Uint63.of_int (1))) f __)
             | EarlyStopSuccess -> (fun _ -> SomeE ())
             | EarlyStopFailure s -> (fun _ -> NoneE s)) __)
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
| _ :: t -> add (Uint63.of_int (1)) (int_len_list t)

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

(** val first_n : 'a1 list -> Uint63.t -> 'a1 list optionE **)

let rec first_n l n0 =
  match l with
  | [] ->
    if eqb n0 (Uint63.of_int (0)) then SomeE [] else NoneE "first_n failure"
  | h :: t ->
    if lesb (Uint63.of_int (1)) n0
    then (match first_n t (sub n0 (Uint63.of_int (1))) with
          | SomeE rec_answer -> SomeE (h :: rec_answer)
          | NoneE err -> NoneE err)
    else SomeE []

(** val last_n : 'a1 list -> Uint63.t -> 'a1 list optionE **)

let last_n l n0 =
  match first_n (rev l) n0 with
  | SomeE aux -> SomeE (rev aux)
  | NoneE err -> NoneE err

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
          then NoneE "Username length must be in range [1..32]"
          else NoneE "Username cannot contain spaces")) __

(** val dummy_username : username **)

let dummy_username =
  "X"

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

(** val deserialize_client_message : bytes -> client_message optionE **)

let deserialize_client_message b = match b with
| [] -> NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
| b0 :: t ->
  (match b0 with
   | '\x00' ->
     (match new_username (string_of_bytes t) with
      | SomeE uname -> SomeE (REG uname)
      | NoneE err -> NoneE err)
   | '\x01' -> SomeE (MESG (string_of_bytes t))
   | '\x02' ->
     (match first_n t (Uint63.of_int (32)) with
      | SomeE name_bytes ->
        (match new_username (string_of_bytes name_bytes) with
         | SomeE uname ->
           (match last_n t (sub (int_len_list t) (Uint63.of_int (32))) with
            | SomeE msg_bytes ->
              SomeE (PMSG ((string_of_bytes msg_bytes), uname))
            | NoneE err -> NoneE err)
         | NoneE err -> NoneE err)
      | NoneE err -> NoneE err)
   | '\x03' ->
     (match new_username (string_of_bytes t) with
      | SomeE uname -> SomeE (EXIT uname)
      | NoneE err -> NoneE err)
   | '\x04' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x05' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x06' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x07' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x08' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\t' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\n' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x0b' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x0c' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\r' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x0e' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x0f' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x10' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x11' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x12' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x13' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x14' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x15' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x16' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x17' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x18' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x19' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x1a' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x1b' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x1c' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x1d' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x1e' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x1f' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | ' ' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '!' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '"' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '#' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '$' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '%' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '&' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\'' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '(' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | ')' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '*' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '+' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | ',' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '-' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '.' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '/' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '0' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '1' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '2' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '3' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '4' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '5' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '6' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '7' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '8' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '9' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | ':' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | ';' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '<' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '=' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '>' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '?' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '@' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'A' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'B' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'C' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'D' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'E' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'F' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'G' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'H' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'I' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'J' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'K' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'L' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'M' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'N' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'O' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'P' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'Q' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'R' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'S' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'T' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'U' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'V' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'W' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'X' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'Y' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'Z' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '[' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\\' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | ']' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '^' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '_' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '`' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'a' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'b' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'c' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'd' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'e' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'f' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'g' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'h' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'i' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'j' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'k' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'l' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'm' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'n' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'o' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'p' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'q' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'r' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 's' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 't' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'u' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'v' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'w' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'x' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'y' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | 'z' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '{' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '|' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '}' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '~' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x7f' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x80' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x81' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x82' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x83' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x84' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x85' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x86' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x87' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x88' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x89' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x8a' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x8b' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x8c' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x8d' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x8e' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x8f' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x90' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x91' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x92' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x93' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x94' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x95' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x96' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x97' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x98' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x99' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x9a' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x9b' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x9c' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x9d' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x9e' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\x9f' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xa0' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xa1' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xa2' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xa3' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xa4' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xa5' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xa6' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xa7' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xa8' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xa9' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xaa' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xab' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xac' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xad' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xae' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xaf' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xb0' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xb1' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xb2' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xb3' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xb4' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xb5' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xb6' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xb7' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xb8' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xb9' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xba' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xbb' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xbc' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xbd' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xbe' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xbf' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xc0' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xc1' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xc2' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xc3' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xc4' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xc5' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xc6' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xc7' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xc8' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xc9' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xca' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xcb' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xcc' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xcd' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xce' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xcf' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xd0' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xd1' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xd2' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xd3' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xd4' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xd5' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xd6' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xd7' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xd8' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xd9' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xda' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xdb' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xdc' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xdd' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xde' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xdf' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xe0' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xe1' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xe2' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xe3' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xe4' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xe5' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xe6' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xe7' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xe8' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xe9' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xea' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xeb' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xec' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xed' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xee' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xef' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xf0' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xf1' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xf2' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xf3' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xf4' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xf5' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xf6' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xf7' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xf8' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xf9' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xfa' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xfb' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xfc' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xfd' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xfe' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b))
   | '\xff' ->
     NoneE ((^) "Client message code not recognized: " (string_of_bytes b)))

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

(** val cc_descr :
    Proofchat.Serverstate.client_connection -> Unix.file_descr **)

let cc_descr = function
| (_, cc_descr0, _) -> cc_descr0

(** val cc_addr : Proofchat.Serverstate.client_connection -> Unix.sockaddr **)

let cc_addr = function
| (_, _, cc_addr0) -> cc_addr0

(** val recv_client_message :
    Proofchat.Serverstate.client_connection -> unit optionE **)

let recv_client_message cc =
  let () = print_endline "Accepted, can receive!" in
  (match recv_message (cc_descr cc) (Uint63.of_int (33)) with
   | SomeE reg_bytes ->
     (match deserialize_client_message reg_bytes with
      | SomeE x ->
        (match x with
         | REG uname ->
           (match Proofchat.Serverstate.get_connection uname with
            | Some _ ->
              let () =
                print_endline
                  ((^) uname " tried to join, but username already taken")
              in
              (match send_message (cc_descr cc)
                       (serialize_server_message (ERR UsernameTaken)) with
               | SomeE _ -> SomeE ()
               | NoneE err -> NoneE err)
            | None ->
              let () = print_endline ((^) uname " has joined") in
              let cc0 = (uname, (cc_descr cc), (cc_addr cc)) in
              let () = Proofchat.Serverstate.add_connection cc0 in SomeE ())
         | MESG _ ->
           send_message (cc_descr cc)
             (serialize_server_message (ERR UnknownMessageFormat))
         | PMSG (_, _) ->
           send_message (cc_descr cc)
             (serialize_server_message (ERR UnknownMessageFormat))
         | EXIT _ ->
           send_message (cc_descr cc)
             (serialize_server_message (ERR UnknownMessageFormat)))
      | NoneE _ ->
        send_message (cc_descr cc)
          (serialize_server_message (ERR UnknownMessageFormat)))
   | NoneE err -> NoneE err)

(** val server_accept_thread : Unix.file_descr -> unit optionE **)

let server_accept_thread socket_fd =
  repeat_until_timeout max_int (fun _ ->
    let (client_descr, client_addr) = Unix.accept socket_fd in
    let () =
      print_endline
        ((^) "Accepted client socket " (string_of_socket_addr client_addr))
    in
    let () =
      keep
        (Thread.create recv_client_message (dummy_username, client_descr,
          client_addr))
    in
    Recurse)

(** val server : string -> int -> unit optionE **)

let server host portno =
  let () = Proofchat.Serverstate.init_connections () in
  let socket_fd =
    Unix.socket Unix.PF_INET Unix.SOCK_STREAM (Uint63.of_int (0))
  in
  let socket_addr = Unix.ADDR_INET ((Unix.inet_addr_of_string host), portno)
  in
  let () =
    print_endline
      ((^) "Binding socket "
        ((^) (string_of_socket_addr socket_addr)
          ((^) " as " (string_of_socket_addr (Unix.getsockname socket_fd)))))
  in
  let () = Unix.bind socket_fd socket_addr in
  let () = Unix.listen socket_fd (Uint63.of_int (10)) in
  let () = print_endline "Server started" in
  (match server_accept_thread socket_fd with
   | SomeE _ ->
     let () = print_endline "Closing server" in
     let () = Unix.close socket_fd in SomeE ()
   | NoneE err -> NoneE err)
