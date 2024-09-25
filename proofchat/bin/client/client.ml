
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

(** val add : nat -> nat -> nat **)

let rec add n0 m =
  match n0 with
  | O -> m
  | S p -> S (add p m)

(** val sub : nat -> nat -> nat **)

let rec sub n0 m =
  match n0 with
  | O -> n0
  | S k -> (match m with
            | O -> n0
            | S l -> sub k l)

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

  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1 **)

  let iter_op op =
    let rec iter p a =
      match p with
      | XI p0 -> op a (iter p0 (op a a))
      | XO p0 -> iter p0 (op a a)
      | XH -> a
    in iter

  (** val to_nat : positive -> nat **)

  let to_nat x =
    iter_op add x (S O)

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

  (** val to_nat : z -> nat **)

  let to_nat = function
  | Z0 -> O
  | Zpos p -> Pos.to_nat p
  | Zneg _ -> O
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



(** val get : nat -> string -> char option **)

let rec get n0 s =
  (* If this appears, you're using String internals. Please don't *)
 (fun f0 f1 s ->
    let l = String.length s in
    if l = 0 then f0 () else f1 (String.get s 0) (String.sub s 1 (l-1)))

    (fun _ -> None)
    (fun c s' -> match n0 with
                 | O -> Some c
                 | S n' -> get n' s')
    s

(** val substring : nat -> nat -> string -> string **)

let rec substring n0 m s =
  match n0 with
  | O ->
    (match m with
     | O -> ""
     | S m' ->
       ((* If this appears, you're using String internals. Please don't *)
 (fun f0 f1 s ->
    let l = String.length s in
    if l = 0 then f0 () else f1 (String.get s 0) (String.sub s 1 (l-1)))

          (fun _ -> s)
          (fun c s' ->
          (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

          (c, (substring O m' s')))
          s))
  | S n' ->
    ((* If this appears, you're using String internals. Please don't *)
 (fun f0 f1 s ->
    let l = String.length s in
    if l = 0 then f0 () else f1 (String.get s 0) (String.sub s 1 (l-1)))

       (fun _ -> s)
       (fun _ s' -> substring n' m s')
       s)

(** val index : nat -> string -> string -> nat option **)

let rec index n0 s1 s2 =
  (* If this appears, you're using String internals. Please don't *)
 (fun f0 f1 s ->
    let l = String.length s in
    if l = 0 then f0 () else f1 (String.get s 0) (String.sub s 1 (l-1)))

    (fun _ ->
    match n0 with
    | O ->
      ((* If this appears, you're using String internals. Please don't *)
 (fun f0 f1 s ->
    let l = String.length s in
    if l = 0 then f0 () else f1 (String.get s 0) (String.sub s 1 (l-1)))

         (fun _ -> Some O)
         (fun _ _ -> None)
         s1)
    | S _ -> None)
    (fun _ s2' ->
    match n0 with
    | O ->
      if (fun s1 s2 ->
     let l1 = String.length s1 and l2 = String.length s2 in
     l1 <= l2 && String.sub s2 0 l1 = s1)
           s1 s2
      then Some O
      else (match index O s1 s2' with
            | Some n1 -> Some (S n1)
            | None -> None)
    | S n' ->
      (match index n' s1 s2' with
       | Some n1 -> Some (S n1)
       | None -> None))
    s2

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

(** val add0 : Uint63.t -> Uint63.t -> Uint63.t **)

let add0 = Uint63.add

(** val sub0 : Uint63.t -> Uint63.t -> Uint63.t **)

let sub0 = Uint63.sub

(** val mul : Uint63.t -> Uint63.t -> Uint63.t **)

let mul = Uint63.mul

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
  sub0 (Uint63.of_int (0)) i

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
  (&&) (lesb (Uint63.of_int (0)) (sub0 n0 (Uint63.of_int (1))))
    (ltsb (sub0 n0 (Uint63.of_int (1))) n0)

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
                    (fun _ _ -> hrec (sub0 timeout (Uint63.of_int (1))) f __)
                  | EarlyStopSuccess -> (fun _ _ -> SomeE ())
                  | EarlyStopFailure s -> (fun _ _ -> NoneE s)) __ __)
             | NoneE s -> (fun _ -> NoneE s)) __)
     else (fun _ -> NoneE "Timeout occurred")) __
  in hrec x x0 __

(** val space : char **)

let space =
  ascii_of_nat (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S O))))))))))))))))))))))))))))))))

(** val space_str : string **)

let space_str =
  (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

    (space, "")

(** val ampersand : char **)

let ampersand =
  ascii_of_nat (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

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
  if ltsb n0 max_int then add0 (Uint63.of_int (1)) n0 else max_int

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
     then (fun _ -> x :: (hrec __ x (sub0 n0 (Uint63.of_int (1))) __))
     else (fun _ -> [])) __
  in hrec __ x0 x1 __

(** val pad_string_r : string -> char -> Uint63.t -> string **)

let pad_string_r s b target_len =
  (^) s (string_of_bytes (create_list b (sub0 target_len (int_len_string s))))

(** val first_n : 'a1 list -> Uint63.t -> 'a1 list optionE **)

let rec first_n l n0 =
  match l with
  | [] ->
    if eqb n0 (Uint63.of_int (0)) then SomeE [] else NoneE "first_n failure"
  | h :: t ->
    if lesb (Uint63.of_int (1)) n0
    then (match first_n t (sub0 n0 (Uint63.of_int (1))) with
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
                 (match last_n l (sub0 (int_len_list l) size0) with
                  | SomeE x3 ->
                    (fun _ ->
                      (match hrec __ x3 size0 (sub0 n0 (Uint63.of_int (1))) __ with
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

(** val resend :
    Uint63.t -> Uint63.t -> Unix.file_descr -> bytes -> Uint63.t -> unit
    optionE **)

let resend x x0 x1 x2 x3 =
  let rec hrec fuel n_sent sockfd message len_msg _ =
    (if sub1_no_underflow fuel
     then (fun _ ->
            (if ltsb
                  (add0 n_sent
                    (send sockfd message n_sent (sub0 len_msg n_sent) []))
                  len_msg
             then (fun _ ->
                    hrec (sub0 fuel (Uint63.of_int (1)))
                      (add0 n_sent
                        (send sockfd message n_sent (sub0 len_msg n_sent) []))
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

(** val recv_server_ACK : Unix.file_descr -> server_message optionE **)

let recv_server_ACK sockfd =
  match recv_int sockfd with
  | SomeE num_users ->
    (match recv_message sockfd (mul num_users (Uint63.of_int (32))) with
     | SomeE usernames_bytes ->
       (match divide usernames_bytes (Uint63.of_int (32)) num_users with
        | SomeE usernames_split ->
          let option_usernames =
            map (fun b -> new_username (string_of_bytes b)) usernames_split
          in
          (match strip_options option_usernames with
           | SomeE usernames -> SomeE (ACK (num_users, usernames))
           | NoneE err -> NoneE err)
        | NoneE err -> NoneE err)
     | NoneE err -> NoneE err)
  | NoneE err -> NoneE err

(** val recv_server_MSG : Unix.file_descr -> server_message optionE **)

let recv_server_MSG sockfd =
  match recv_username sockfd with
  | SomeE username0 ->
    (match recv_string sockfd with
     | SomeE msg -> SomeE (MSG (username0, msg))
     | NoneE err -> NoneE err)
  | NoneE err -> NoneE err

(** val recv_server_ERR : Unix.file_descr -> server_message optionE **)

let recv_server_ERR sockfd =
  match recv_int sockfd with
  | SomeE err_code -> SomeE (ERR (error_of_int err_code))
  | NoneE err -> NoneE err

(** val recv_server_message : Unix.file_descr -> server_message optionE **)

let recv_server_message sockfd =
  match recv_message sockfd (Uint63.of_int (1)) with
  | SomeE code ->
    (match code with
     | [] ->
       NoneE
         ((^) "Failed to receieve server message with opcode "
           (string_of_bytes code))
     | b :: l ->
       (match b with
        | '\x00' ->
          (match l with
           | [] -> recv_server_ACK sockfd
           | _ :: _ ->
             NoneE
               ((^) "Failed to receieve server message with opcode "
                 (string_of_bytes code)))
        | '\x01' ->
          (match l with
           | [] -> recv_server_MSG sockfd
           | _ :: _ ->
             NoneE
               ((^) "Failed to receieve server message with opcode "
                 (string_of_bytes code)))
        | '\x02' ->
          (match l with
           | [] -> recv_server_ERR sockfd
           | _ :: _ ->
             NoneE
               ((^) "Failed to receieve server message with opcode "
                 (string_of_bytes code)))
        | '\x03' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x04' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x05' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x06' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x07' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x08' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\t' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\n' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x0b' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x0c' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\r' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x0e' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x0f' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x10' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x11' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x12' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x13' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x14' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x15' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x16' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x17' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x18' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x19' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x1a' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x1b' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x1c' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x1d' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x1e' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x1f' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | ' ' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '!' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '"' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '#' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '$' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '%' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '&' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\'' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '(' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | ')' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '*' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '+' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | ',' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '-' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '.' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '/' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '0' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '1' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '2' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '3' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '4' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '5' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '6' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '7' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '8' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '9' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | ':' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | ';' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '<' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '=' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '>' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '?' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '@' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'A' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'B' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'C' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'D' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'E' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'F' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'G' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'H' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'I' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'J' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'K' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'L' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'M' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'N' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'O' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'P' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'Q' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'R' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'S' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'T' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'U' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'V' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'W' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'X' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'Y' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'Z' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '[' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\\' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | ']' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '^' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '_' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '`' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'a' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'b' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'c' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'd' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'e' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'f' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'g' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'h' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'i' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'j' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'k' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'l' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'm' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'n' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'o' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'p' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'q' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'r' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 's' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 't' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'u' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'v' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'w' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'x' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'y' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | 'z' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '{' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '|' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '}' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '~' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x7f' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x80' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x81' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x82' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x83' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x84' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x85' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x86' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x87' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x88' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x89' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x8a' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x8b' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x8c' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x8d' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x8e' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x8f' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x90' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x91' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x92' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x93' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x94' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x95' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x96' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x97' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x98' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x99' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x9a' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x9b' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x9c' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x9d' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x9e' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\x9f' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xa0' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xa1' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xa2' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xa3' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xa4' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xa5' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xa6' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xa7' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xa8' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xa9' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xaa' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xab' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xac' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xad' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xae' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xaf' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xb0' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xb1' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xb2' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xb3' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xb4' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xb5' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xb6' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xb7' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xb8' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xb9' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xba' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xbb' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xbc' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xbd' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xbe' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xbf' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xc0' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xc1' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xc2' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xc3' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xc4' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xc5' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xc6' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xc7' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xc8' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xc9' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xca' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xcb' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xcc' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xcd' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xce' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xcf' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xd0' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xd1' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xd2' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xd3' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xd4' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xd5' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xd6' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xd7' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xd8' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xd9' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xda' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xdb' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xdc' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xdd' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xde' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xdf' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xe0' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xe1' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xe2' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xe3' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xe4' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xe5' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xe6' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xe7' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xe8' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xe9' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xea' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xeb' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xec' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xed' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xee' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xef' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xf0' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xf1' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xf2' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xf3' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xf4' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xf5' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xf6' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xf7' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xf8' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xf9' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xfa' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xfb' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xfc' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xfd' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xfe' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))
        | '\xff' ->
          NoneE
            ((^) "Failed to receieve server message with opcode "
              (string_of_bytes code))))
  | NoneE err -> NoneE err

(** val client_send_thread : Unix.file_descr -> unit optionE **)

let client_send_thread sockfd =
  repeat_until_timeout max_int (fun _ ->
    let () = (fun s -> print_string s; flush stdout) ">>> " in
    (match (fun _ -> SomeE (read_line ())) () with
     | SomeE msg ->
       let () = (fun s -> print_endline s; flush stdout) "read line" in
       (match if ltsb (Uint63.of_int (1)) (int_len_string msg)
              then let first_char =
                     match get O msg with
                     | Some a -> a
                     | None -> space
                   in
                   if (=) first_char ampersand
                   then let split_idx =
                          match index O space_str msg with
                          | Some n0 -> n0
                          | None -> O
                        in
                        let msg_text =
                          substring (add split_idx (S O))
                            (sub (Z.to_nat (to_Z0 (int_len_string msg)))
                              split_idx) msg
                        in
                        let uname_text =
                          substring (S O) (sub split_idx (S O)) msg
                        in
                        (match new_username uname_text with
                         | SomeE uname ->
                           send_message sockfd
                             (serialize_client_message (PMSG (msg_text,
                               uname)))
                         | NoneE err -> NoneE err)
                   else send_message sockfd
                          (serialize_client_message (MESG msg))
              else SomeE () with
        | SomeE _ -> SomeE Recurse
        | NoneE s ->
          let () = Proofchat.Logging._log Log_Error s in SomeE Recurse)
     | NoneE err -> NoneE err))

(** val client_recv_thread : Unix.file_descr -> unit optionE **)

let client_recv_thread sockfd =
  repeat_until_timeout max_int (fun _ ->
    match recv_server_message sockfd with
    | SomeE server_msg ->
      let () =
        match server_msg with
        | ACK (_, _) -> ()
        | MSG (uname, msg) ->
          let () =
            (fun s -> print_endline s; flush stdout)
              ((^) uname ((^) ": " msg))
          in
          (fun s -> print_string s; flush stdout) ">>> "
        | ERR err ->
          let () = (fun s -> print_endline s; flush stdout) "" in
          let () = Proofchat.Logging._log Log_Error (string_of_error err) in
          (fun s -> print_string s; flush stdout) ">>> "
      in
      SomeE Recurse
    | NoneE err -> NoneE err)

(** val client : string -> int -> unit optionE **)

let client host portno =
  let () =
    (fun s -> print_endline s; flush stdout)
      "Please enter a username (length 1-32, no spaces)"
  in
  (match (fun _ -> SomeE (read_line ())) () with
   | SomeE username_string ->
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
                ((^) " as "
                  (string_of_socket_addr (Unix.getsockname socket_fd)))))
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
                 let () =
                   Proofchat.Logging._log Log_Info
                     "Server accepted connection"
                 in
                 let () =
                   Proofchat.Logging._log Log_Info
                     ((^) "Total users: " (string_of_int num_users))
                 in
                 (match (fun a b -> SomeE (Thread.create a b))
                          client_send_thread socket_fd with
                  | SomeE input_thread ->
                    (match (fun a b -> SomeE (Thread.create a b))
                             client_recv_thread socket_fd with
                     | SomeE _ ->
                       let () = Thread.join input_thread in
                       let () =
                         Proofchat.Logging._log Log_Info
                           "Closing connection to server"
                       in
                       let () = Unix.close socket_fd in SomeE ()
                     | NoneE err -> NoneE err)
                  | NoneE err -> NoneE err)
               | NoneE err -> NoneE err)
            | NoneE err -> NoneE err)
         | NoneE err -> NoneE err)
      | NoneE err -> NoneE err)
   | NoneE err -> NoneE err)
