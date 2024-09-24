
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

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
  | XI p -> XI (XO p)
  | XO p -> XI (pred_double p)
  | XH -> XH

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

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let map f =
  let rec map0 = function
  | [] -> []
  | a :: t -> (f a) :: (map0 t)
  in map0

module Z =
 struct
  (** val double : z -> z **)

  let double = function
  | Z0 -> Z0
  | Zpos p -> Zpos (XO p)
  | Zneg p -> Zneg (XO p)

  (** val succ_double : z -> z **)

  let succ_double = function
  | Z0 -> Zpos XH
  | Zpos p -> Zpos (XI p)
  | Zneg p -> Zneg (Pos.pred_double p)

  (** val opp : z -> z **)

  let opp = function
  | Z0 -> Z0
  | Zpos x0 -> Zneg x0
  | Zneg x0 -> Zpos x0
 end

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

(** val strip_options : 'a1 optionE list -> 'a1 list optionE **)

let rec strip_options = function
| [] -> SomeE []
| o :: t ->
  (match o with
   | SomeE a ->
     (match strip_options t with
      | SomeE t' -> SomeE (a :: t')
      | NoneE err -> NoneE err)
   | NoneE s -> NoneE ((^) "strip_options fail: " s))

(** val lsr0 : Uint63.t -> Uint63.t -> Uint63.t **)

let lsr0 = Uint63.l_sr

(** val land0 : Uint63.t -> Uint63.t -> Uint63.t **)

let land0 = Uint63.l_and

(** val add : Uint63.t -> Uint63.t -> Uint63.t **)

let add = Uint63.add

(** val sub : Uint63.t -> Uint63.t -> Uint63.t **)

let sub = Uint63.sub

(** val eqb : Uint63.t -> Uint63.t -> bool **)

let eqb = Uint63.equal

(** val ltb : Uint63.t -> Uint63.t -> bool **)

let ltb = Uint63.lt

(** val ltsb : Uint63.t -> Uint63.t -> bool **)

let ltsb = Uint63.lts

(** val lesb : Uint63.t -> Uint63.t -> bool **)

let lesb = Uint63.les

(** val size : nat **)

let size =
  S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val is_zero : Uint63.t -> bool **)

let is_zero i =
  eqb i (Uint63.of_int (0))

(** val is_even : Uint63.t -> bool **)

let is_even i =
  is_zero (land0 i (Uint63.of_int (1)))

(** val opp0 : Uint63.t -> Uint63.t **)

let opp0 i =
  sub (Uint63.of_int (0)) i

(** val to_Z_rec : nat -> Uint63.t -> z **)

let rec to_Z_rec n0 i =
  match n0 with
  | O -> Z0
  | S n1 ->
    (if is_even i then Z.double else Z.succ_double)
      (to_Z_rec n1 (lsr0 i (Uint63.of_int (1))))

(** val to_Z : Uint63.t -> z **)

let to_Z =
  to_Z_rec size

(** val min_int : Uint63.t **)

let min_int =
  (Uint63.of_int (-4611686018427387904))

(** val max_int : Uint63.t **)

let max_int =
  (Uint63.of_int (4611686018427387903))

(** val to_Z0 : Uint63.t -> z **)

let to_Z0 i =
  if ltb i min_int then to_Z i else Z.opp (to_Z (opp0 i))

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

(** val divide : 'a1 list -> Uint63.t -> Uint63.t -> 'a1 list list optionE **)

let divide x0 x1 x2 =
  let rec hrec _ l size0 n0 _ =
    (if sub1_no_underflow n0
     then (fun _ ->
            (match first_n l size0 with
             | SomeE x ->
               (fun _ ->
                 (match last_n l (sub (int_len_list l) size0) with
                  | SomeE x3 ->
                    (fun _ ->
                      (match hrec __ x3 size0 (sub n0 (Uint63.of_int (1))) __ with
                       | SomeE x4 -> (fun _ _ -> SomeE (x :: x4))
                       | NoneE s -> (fun _ _ -> NoneE s)) __ __)
                  | NoneE s -> (fun _ -> NoneE s)) __)
             | NoneE s -> (fun _ -> NoneE s)) __)
     else (fun _ -> SomeE [])) __
  in hrec __ x0 x1 x2 __

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

(** val serialize_client_message : client_message -> bytes **)

let serialize_client_message = function
| REG name -> '\x00' :: (serialize_username name)
| MESG msg -> '\x01' :: (serialize_string msg)
| PMSG (message, name) ->
  '\x02' :: (app (serialize_username name) (serialize_string message))
| EXIT name -> '\x03' :: (serialize_username name)

type error =
| UsernameTaken
| UsernameTooLong
| UsernameHasSpaces
| PmsgTargetNotExists
| UnknownMessageFormat
| Error

(** val string_of_error : error -> string **)

let string_of_error = function
| UsernameTaken -> "This username is already taken"
| UsernameTooLong -> "Username length must be in range [1..32]"
| UsernameHasSpaces -> "Username cannot contain spaces"
| PmsgTargetNotExists -> "The target of your private message does not exist"
| UnknownMessageFormat -> "Received an unknown message type"
| Error -> "Unknown error occurred"

(** val error_of_int : Uint63.t -> error **)

let error_of_int n0 =
  match to_Z0 n0 with
  | Z0 -> UsernameTaken
  | Zpos p ->
    (match p with
     | XI p0 ->
       (match p0 with
        | XI _ -> Error
        | XO _ -> Error
        | XH -> PmsgTargetNotExists)
     | XO p0 ->
       (match p0 with
        | XI _ -> Error
        | XO p1 ->
          (match p1 with
           | XI _ -> Error
           | XO _ -> Error
           | XH -> UnknownMessageFormat)
        | XH -> UsernameHasSpaces)
     | XH -> UsernameTooLong)
  | Zneg _ -> Error

type server_message =
| ACK of Uint63.t * username list
| MSG of username * string
| ERR of error

(** val deserialize_server_message : bytes -> server_message optionE **)

let deserialize_server_message b = match b with
| [] -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
| b0 :: t ->
  (match b0 with
   | '\x00' ->
     (match first_n t (Uint63.of_int (8)) with
      | SomeE num_users_bytes ->
        (match last_n t (sub (int_len_list t) (Uint63.of_int (8))) with
         | SomeE _ ->
           let num_users = Proofchat.Pfbytes.bytes_to_int63 num_users_bytes in
           (match divide t (Uint63.of_int (32)) num_users with
            | SomeE usernames_bytes ->
              let option_usernames =
                map (fun b1 -> new_username (string_of_bytes b1))
                  usernames_bytes
              in
              (match strip_options option_usernames with
               | SomeE usernames -> SomeE (ACK (num_users, usernames))
               | NoneE err -> NoneE err)
            | NoneE err -> NoneE err)
         | NoneE err -> NoneE err)
      | NoneE err -> NoneE err)
   | '\x01' ->
     (match first_n t (Uint63.of_int (32)) with
      | SomeE username_bytes ->
        (match new_username (string_of_bytes username_bytes) with
         | SomeE uname ->
           (match last_n t (sub (int_len_list t) (Uint63.of_int (32))) with
            | SomeE t0 ->
              (match first_n t0 (Uint63.of_int (8)) with
               | SomeE len_msg_bytes ->
                 let len_msg = Proofchat.Pfbytes.bytes_to_int63 len_msg_bytes
                 in
                 (match last_n t0 len_msg with
                  | SomeE msg_bytes ->
                    SomeE (MSG (uname, (string_of_bytes msg_bytes)))
                  | NoneE err -> NoneE err)
               | NoneE err -> NoneE err)
            | NoneE err -> NoneE err)
         | NoneE err -> NoneE err)
      | NoneE err -> NoneE err)
   | '\x02' ->
     (match first_n t (Uint63.of_int (8)) with
      | SomeE err_bytes ->
        SomeE (ERR
          (error_of_int (Proofchat.Pfbytes.bytes_to_int63 err_bytes)))
      | NoneE err -> NoneE err)
   | '\x03' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x04' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x05' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x06' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x07' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x08' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\t' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\n' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x0b' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x0c' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\r' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x0e' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x0f' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x10' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x11' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x12' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x13' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x14' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x15' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x16' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x17' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x18' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x19' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x1a' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x1b' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x1c' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x1d' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x1e' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x1f' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | ' ' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '!' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '"' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '#' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '$' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '%' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '&' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\'' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '(' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | ')' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '*' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '+' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | ',' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '-' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '.' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '/' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '0' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '1' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '2' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '3' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '4' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '5' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '6' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '7' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '8' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '9' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | ':' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | ';' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '<' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '=' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '>' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '?' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '@' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'A' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'B' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'C' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'D' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'E' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'F' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'G' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'H' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'I' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'J' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'K' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'L' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'M' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'N' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'O' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'P' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'Q' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'R' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'S' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'T' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'U' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'V' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'W' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'X' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'Y' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'Z' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '[' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\\' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | ']' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '^' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '_' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '`' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'a' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'b' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'c' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'd' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'e' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'f' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'g' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'h' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'i' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'j' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'k' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'l' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'm' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'n' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'o' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'p' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'q' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'r' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 's' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 't' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'u' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'v' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'w' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'x' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'y' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | 'z' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '{' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '|' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '}' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '~' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x7f' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x80' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x81' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x82' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x83' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x84' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x85' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x86' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x87' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x88' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x89' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x8a' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x8b' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x8c' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x8d' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x8e' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x8f' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x90' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x91' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x92' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x93' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x94' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x95' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x96' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x97' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x98' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x99' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x9a' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x9b' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x9c' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x9d' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x9e' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\x9f' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xa0' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xa1' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xa2' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xa3' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xa4' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xa5' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xa6' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xa7' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xa8' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xa9' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xaa' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xab' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xac' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xad' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xae' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xaf' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xb0' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xb1' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xb2' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xb3' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xb4' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xb5' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xb6' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xb7' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xb8' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xb9' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xba' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xbb' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xbc' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xbd' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xbe' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xbf' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xc0' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xc1' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xc2' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xc3' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xc4' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xc5' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xc6' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xc7' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xc8' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xc9' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xca' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xcb' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xcc' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xcd' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xce' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xcf' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xd0' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xd1' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xd2' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xd3' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xd4' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xd5' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xd6' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xd7' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xd8' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xd9' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xda' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xdb' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xdc' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xdd' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xde' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xdf' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xe0' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xe1' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xe2' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xe3' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xe4' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xe5' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xe6' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xe7' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xe8' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xe9' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xea' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xeb' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xec' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xed' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xee' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xef' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xf0' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xf1' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xf2' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xf3' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xf4' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xf5' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xf6' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xf7' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xf8' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xf9' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xfa' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xfb' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xfc' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xfd' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xfe' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b))
   | '\xff' -> NoneE ((^) "Failed to deserialize bytes: " (string_of_bytes b)))

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

(** val max_message_len : Uint63.t **)

let max_message_len =
  (Uint63.of_int (4096))

(** val send_message : Unix.file_descr -> bytes -> unit optionE **)

let send_message sockfd message =
  if ltsb max_message_len (int_len_list message)
  then NoneE "Messages cannot exceed 4kb"
  else resend (Uint63.of_int (100)) (Uint63.of_int (0)) sockfd message
         (int_len_list message)

(** val recv_message : Unix.file_descr -> Uint63.t -> bytes optionE **)

let recv_message sockfd len =
  let (_, out) = recv sockfd (Uint63.of_int (0)) len [] in SomeE out

(** val recv_server_message : Unix.file_descr -> server_message optionE **)

let recv_server_message sockfd =
  match recv_message sockfd max_message_len with
  | SomeE msg_bytes -> deserialize_server_message msg_bytes
  | NoneE err -> NoneE err

(** val client : string -> int -> unit optionE **)

let client host portno =
  let () = print_endline "Please enter a username (length 1-32, no spaces)" in
  let username_string = read_line () in
  (match new_username username_string with
   | SomeE uname ->
     let socket_fd =
       Unix.socket Unix.PF_INET Unix.SOCK_STREAM (Uint63.of_int (0))
     in
     let socket_addr = Unix.ADDR_INET ((Unix.inet_addr_of_string host),
       portno)
     in
     let () =
       Proofchat.Logging._log Log_Debug
         ((^) "Opening client connection to "
           ((^) (string_of_socket_addr socket_addr)
             ((^) " as " (string_of_socket_addr (Unix.getsockname socket_fd)))))
     in
     let () = Unix.connect socket_fd socket_addr in
     (match send_message socket_fd (serialize_client_message (REG uname)) with
      | SomeE _ ->
        (match recv_server_message socket_fd with
         | SomeE server_ack ->
           (match match server_ack with
                  | ACK (num_users, _) -> SomeE num_users
                  | MSG (_, _) -> NoneE "Server denied connection"
                  | ERR s -> NoneE (string_of_error s) with
            | SomeE num_users ->
              let () = print_endline "what" in
              let () =
                Proofchat.Logging._log Log_Info "Server accepted connection"
              in
              let () =
                Proofchat.Logging._log Log_Info
                  ((^) (string_of_int num_users) " other users connected")
              in
              let () = Unix.sleep (Uint63.of_int (1000)) in
              let () = print_endline "Closing connection to server" in
              let () = Unix.close socket_fd in SomeE ()
            | NoneE err -> NoneE err)
         | NoneE err -> NoneE err)
      | NoneE err -> NoneE err)
   | NoneE err -> NoneE err)
