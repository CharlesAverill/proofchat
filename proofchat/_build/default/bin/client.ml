
let __ = let rec f _ = Obj.repr f in Obj.repr f

type nat =
| O
| S of nat

type comparison =
| Eq
| Lt
| Gt

(** val compOpp : comparison -> comparison **)

let compOpp = function
| Eq -> Eq
| Lt -> Gt
| Gt -> Lt

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

  (** val compare_cont : comparison -> positive -> positive -> comparison **)

  let rec compare_cont r x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> compare_cont r p q
       | XO q -> compare_cont Gt p q
       | XH -> Gt)
    | XO p ->
      (match y with
       | XI q -> compare_cont Lt p q
       | XO q -> compare_cont r p q
       | XH -> Gt)
    | XH -> (match y with
             | XI _ -> Lt
             | XO _ -> Lt
             | XH -> r)

  (** val compare : positive -> positive -> comparison **)

  let compare =
    compare_cont Eq

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

module Z =
 struct
  (** val compare : z -> z -> comparison **)

  let compare x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> Eq
             | Zpos _ -> Lt
             | Zneg _ -> Gt)
    | Zpos x' ->
      (match y with
       | Z0 -> Gt
       | Zpos y' -> Pos.compare x' y'
       | Zneg _ -> Gt)
    | Zneg x' ->
      (match y with
       | Z0 -> Lt
       | Zpos _ -> Lt
       | Zneg y' -> compOpp (Pos.compare x' y'))

  (** val leb : z -> z -> bool **)

  let leb x y =
    match compare x y with
    | Eq -> true
    | Lt -> true
    | Gt -> false

  (** val of_nat : nat -> z **)

  let of_nat = function
  | O -> Z0
  | S n1 -> Zpos (Pos.of_succ_nat n1)
 end

(** val length : string -> nat **)

let rec length s =
  (* If this appears, you're using String internals. Please don't *)
 (fun f0 f1 s ->
    let l = String.length s in
    if l = 0 then f0 () else f1 (String.get s 0) (String.sub s 1 (l-1)))

    (fun _ -> O)
    (fun _ s' -> S (length s'))
    s

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

type 'x optionE =
| SomeE of 'x
| NoneE of string



type username =
  string
  (* singleton inductive, whose constructor was Build_username *)

(** val validate_username : string -> bool **)

let validate_username name =
  (&&)
    ((&&) (Z.leb (Zpos XH) (Z.of_nat (length name)))
      (Z.leb (Z.of_nat (length name)) (Zpos (XO (XO (XO (XO (XO XH))))))))
    (no_spaces name)

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

(** val print_socket_addr : Unix.sockaddr -> unit **)

let print_socket_addr = function
| Unix.ADDR_UNIX s' -> (fun s -> print_string s; flush stdout) s'
| Unix.ADDR_INET (addr, p) ->
  (fun x y -> x; y)
    ((fun x y -> x; y)
      ((fun s -> print_string s; flush stdout)
        (Unix.string_of_inet_addr addr))
      ((fun s -> print_string s; flush stdout) ":")) (print_int p)

(** val client : string -> int -> unit optionE **)

let client host portno =
  (fun x y -> x; y)
    ((fun s -> print_endline s; flush stdout)
      "Please enter a username (length 1-32, no spaces)")
    (let username_string = read_line () in
     match new_username username_string with
     | SomeE _ ->
       let socket_fd =
         Unix.socket Unix.PF_INET Unix.SOCK_STREAM (Uint63.of_int (-1))
       in
       let socket_addr = Unix.ADDR_INET ((Unix.inet_addr_of_string host),
         portno)
       in
       (fun x y -> x; y)
         ((fun x y -> x; y)
           ((fun x y -> x; y)
             ((fun x y -> x; y)
               ((fun x y -> x; y)
                 ((fun s -> print_string s; flush stdout)
                   "Opening client connection to ")
                 (print_socket_addr socket_addr))
               ((fun s -> print_string s; flush stdout) " as "))
             (print_socket_addr (Unix.getsockname socket_fd)))
           ((fun s -> print_endline s; flush stdout) "")) (SomeE ())
     | NoneE err -> NoneE err)
