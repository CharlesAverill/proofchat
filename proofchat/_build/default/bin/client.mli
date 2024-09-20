
type nat =
| O
| S of nat

type comparison =
| Eq
| Lt
| Gt

val compOpp : comparison -> comparison

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

module Pos :
 sig
  val succ : positive -> positive

  val compare_cont : comparison -> positive -> positive -> comparison

  val compare : positive -> positive -> comparison

  val of_succ_nat : nat -> positive
 end

module N :
 sig
  val of_nat : nat -> n
 end

val zero : char

val one : char

val shift : bool -> char -> char

val ascii_of_pos : positive -> char

val ascii_of_N : n -> char

val ascii_of_nat : nat -> char

module Z :
 sig
  val compare : z -> z -> comparison

  val leb : z -> z -> bool

  val of_nat : nat -> z
 end

val length : string -> nat

val space : char

val no_spaces : string -> bool

type 'x optionE =
| SomeE of 'x
| NoneE of string



type username =
  string
  (* singleton inductive, whose constructor was Build_username *)

val validate_username : string -> bool

val new_username : string -> username optionE

val print_socket_addr : Unix.sockaddr -> unit

val client : string -> int -> unit optionE
