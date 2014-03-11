(***********************************************************************)
(*                                                                     *)
(*                                Ulib                                 *)
(*                                                                     *)
(*                   OCaml user's additional library                   *)
(*                                                                     *)
(*               EPI Pomdapi, INRIA Paris - Rocquencourt               *)
(*                                                                     *)
(*  Copyright 2004-2013 INRIA.                                         *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved. This file is distributed only by permission.  *)
(*                                                                     *)
(*  Francois Clement <Francois.Clement@inria.fr>                       *)
(*  Pierre Weis <Pierre.Weis@inria.fr>                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** {3 Natural numbers} *)

type t = Unatural_types.natural;;
(** The type of natural numbers. *)

val _0_ : t;;
val _1_ : t;;
val _2_ : t;;
val _3_ : t;;
val _4_ : t;;
val _5_ : t;;
val _6_ : t;;
val _7_ : t;;
val _8_ : t;;
val _9_ : t;;
(** The naturals 0 to 9. *)

val add : t -> t -> t;;
(** Addition. *)

val sub : t -> t -> t;;
(** Subtraction. *)

val mul : t -> t -> t;;
(** Multiplication. *)

val div : t -> t -> t;;
(** Integer division. This division rounds the real quotient of its arguments
   towards zero, as specified for [Pervasives.( / )].
   
   Raise [Division_by_zero] if the second argument is {!_0_}. *)

val rem : t -> t -> t;;
(** Integer remainder. If [y] is not {!_0_}, the result of [rem x y]
   satisfies the following property:
   [x =] {!add} [(]{!mul} [(]{!div} [x y) y) (]{!rem} [x y)].
   
   Raise [Division_by_zero] if the second argument is {!_0_}. *)

val gcd : t -> t -> t;;
(** Greatest common divisor. *)

val succ : t -> t
(** Successor. [succ x] is {!add} [x] {!_1_}. *)

val pred : t -> t
(** Predecessor. [pred x] is {!sub} [x] {!_1_}. *)

val max_nat : t
(** The greatest representable natural, {!of_int} [Pervasives.max_int]. *)

val min_nat : t
(** The smallest representable natural, {!_0_}. *)

val of_int : int -> t;;
(** Convert the given integer into a natural. *)

val to_int : t -> int;;
(** Convert the given natural into an integer. *)

val of_float : float -> t;;
(** Convert the given floating-point number to a natural, discarding the
   fractional part (round towards zero). The result of the conversion is
   undefined if, after truncation, the number is outside the range
   \[{!min_nat}..{!max_nat}\]. *)

val to_float : t -> float;;
(** Convert the given natural to a floating-point number. *)

val of_string : string -> t;;
(** Convert the given string to a natural. The string is read in decimal
   (by default) or in hexadecimal, octal or binary if it begins with ["0x"],
   ["0o"] or ["0b"] respectively.
   
   Raise [Failure] if the given string is not a valid representation of an
   integer, or if the integer represented is outside of the range of
   representable naturals \[{!min_nat}..{!max_nat}\]. *)

val to_string : t -> string;;
(** Return the string representation of its argument, in decimal. *)

val compare : t -> t -> int;;
(** The comparison function for naturals, with the same specification as
    [Pervasives.compare]. Along with the type [t], this function [compare]
    allows the module [Unatural] to be passed as argument to the functors
    [Set.Make] and [Map.Make]. *)

val split : t -> t -> t list;;
(** [split n k] returns a list of [k] integers that differs at most by 1
  and such that their sum equals [n]. *)

val psplit : t -> t -> t list;;
(** Same as [split] but the result is a list of at most [k] positive
 integers. *)

(**/**)
(** {6 Undocumented functions} *)

external unsafe_of_int : int -> t = "%identity";;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
