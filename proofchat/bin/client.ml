
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

type bytes = char list

(** val send :
    Unix.file_descr -> bytes -> Uint63.t -> Uint63.t -> Unix.msg_flag list ->
    Uint63.t **)

let send = (fun a b c d e -> Unix.send a (Pfbytes.bytes_of_byte_list b) c d e)

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
| _ :: t -> add (Uint63.of_int (1)) (int_len_list t)

(** val bytes_of_string : string -> bytes **)

let rec bytes_of_string s =
  (* If this appears, you're using String internals. Please don't *)
 (fun f0 f1 s ->
    let l = String.length s in
    if l = 0 then f0 () else f1 (String.get s 0) (String.sub s 1 (l-1)))

    (fun _ -> '\x00' :: [])
    (fun a s' -> ((fun x -> x) a) :: (bytes_of_string s'))
    s

(** val string_of_bytes : bytes -> string **)

let rec string_of_bytes = function
| [] -> ""
| h :: t ->
  (match h with
   | '\x00' ->
     (match t with
      | [] -> ""
      | _ :: _ ->
        (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

          (((fun x -> x) h), (string_of_bytes t)))
   | '\x01' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x02' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x03' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x04' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x05' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x06' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x07' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x08' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\t' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\n' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x0b' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x0c' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\r' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x0e' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x0f' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x10' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x11' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x12' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x13' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x14' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x15' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x16' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x17' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x18' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x19' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x1a' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x1b' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x1c' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x1d' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x1e' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x1f' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | ' ' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '!' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '"' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '#' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '$' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '%' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '&' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\'' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '(' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | ')' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '*' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '+' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | ',' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '-' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '.' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '/' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '0' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '1' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '2' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '3' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '4' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '5' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '6' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '7' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '8' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '9' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | ':' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | ';' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '<' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '=' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '>' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '?' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '@' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'A' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'B' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'C' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'D' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'E' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'F' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'G' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'H' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'I' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'J' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'K' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'L' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'M' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'N' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'O' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'P' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'Q' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'R' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'S' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'T' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'U' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'V' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'W' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'X' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'Y' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'Z' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '[' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\\' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | ']' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '^' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '_' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '`' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'a' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'b' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'c' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'd' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'e' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'f' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'g' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'h' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'i' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'j' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'k' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'l' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'm' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'n' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'o' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'p' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'q' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'r' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 's' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 't' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'u' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'v' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'w' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'x' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'y' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | 'z' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '{' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '|' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '}' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '~' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x7f' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x80' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x81' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x82' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x83' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x84' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x85' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x86' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x87' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x88' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x89' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x8a' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x8b' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x8c' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x8d' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x8e' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x8f' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x90' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x91' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x92' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x93' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x94' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x95' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x96' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x97' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x98' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x99' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x9a' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x9b' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x9c' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x9d' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x9e' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\x9f' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xa0' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xa1' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xa2' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xa3' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xa4' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xa5' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xa6' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xa7' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xa8' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xa9' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xaa' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xab' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xac' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xad' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xae' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xaf' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xb0' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xb1' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xb2' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xb3' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xb4' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xb5' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xb6' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xb7' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xb8' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xb9' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xba' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xbb' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xbc' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xbd' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xbe' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xbf' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xc0' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xc1' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xc2' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xc3' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xc4' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xc5' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xc6' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xc7' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xc8' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xc9' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xca' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xcb' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xcc' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xcd' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xce' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xcf' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xd0' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xd1' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xd2' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xd3' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xd4' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xd5' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xd6' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xd7' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xd8' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xd9' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xda' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xdb' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xdc' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xdd' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xde' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xdf' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xe0' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xe1' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xe2' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xe3' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xe4' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xe5' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xe6' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xe7' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xe8' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xe9' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xea' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xeb' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xec' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xed' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xee' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xef' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xf0' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xf1' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xf2' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xf3' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xf4' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xf5' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xf6' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xf7' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xf8' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xf9' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xfa' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xfb' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xfc' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xfd' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xfe' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t))
   | '\xff' ->
     (* If this appears, you're using String internals. Please don't *)
  (fun (c, s) -> String.make 1 c ^ s)

       (((fun x -> x) h), (string_of_bytes t)))

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
  (^) s
    (string_of_bytes
      (create_list b
        (add (sub target_len (int_len_string s)) (Uint63.of_int (1)))))

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

type client_message =
| REG of username
| MESG of string
| PMSG of string * username
| EXIT of username

(** val serialize_string : string -> bytes **)

let serialize_string s =
  app (Pfbytes.int63_to_bytes (int_len_string s)) (bytes_of_string s)

(** val serialize_username : username -> bytes **)

let serialize_username u =
  bytes_of_string (pad_string_r u '\x00' (Uint63.of_int (32)))

(** val serialize_client_message : client_message -> bytes **)

let serialize_client_message = function
| REG name -> '\x00' :: (serialize_username name)
| MESG msg -> '\x01' :: (serialize_string msg)
| PMSG (message, name) ->
  '\x02' :: (app (serialize_username name) (serialize_string message))
| EXIT name -> '\x03' :: (serialize_username name)

(** val resend :
    Uint63.t -> Uint63.t -> Unix.file_descr -> bytes -> Uint63.t -> unit
    optionE **)

let resend x x0 x1 x2 x3 =
  let rec hrec fuel n_sent sockfd message len_msg _ =
    (if sub1_no_underflow fuel
     then (fun _ ->
            (if lesb
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
  let len_msg = int_len_list message in
  let n_sent = send sockfd message (Uint63.of_int (0)) len_msg [] in
  if lesb n_sent len_msg
  then resend (Uint63.of_int (100)) n_sent sockfd message len_msg
  else SomeE ()

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
       print_endline
         ((^) "Opening client connection to "
           ((^) (string_of_socket_addr socket_addr)
             ((^) " as " (string_of_socket_addr (Unix.getsockname socket_fd)))))
     in
     let () = Unix.connect socket_fd socket_addr in
     (match send_message socket_fd (serialize_client_message (REG uname)) with
      | SomeE _ ->
        let () = print_endline "Closing client connection" in
        let () = Unix.close socket_fd in SomeE ()
      | NoneE err -> NoneE err)
   | NoneE err -> NoneE err)
