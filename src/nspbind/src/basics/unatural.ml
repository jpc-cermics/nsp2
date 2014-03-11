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

(* Natural numbers. *)

type t = Unatural_types.natural;;

let of_int = Unatural_types.to_natural
and to_int = Unatural_types.of_natural
;;

external unsafe_of_int : int -> t = "%identity";;

let _0_ = unsafe_of_int 0;;
let _1_ = unsafe_of_int 1;;
let _2_ = unsafe_of_int 2;;
let _3_ = unsafe_of_int 3;;
let _4_ = unsafe_of_int 4;;
let _5_ = unsafe_of_int 5;;
let _6_ = unsafe_of_int 6;;
let _7_ = unsafe_of_int 7;;
let _8_ = unsafe_of_int 8;;
let _9_ = unsafe_of_int 9;;

let lift f x = of_int (f (to_int x));;
let lift2 f x y = of_int (f (to_int x) (to_int y));;

let add = lift2 Pervasives.( + );;
let sub = lift2 Pervasives.( - );;
let mul = lift2 Pervasives.( * );;
let div = lift2 Pervasives.( / );;
let rem = lift2 Pervasives.( mod );;

let rec gcd p q = if q = _0_ then p else gcd q (rem p q);;

let succ = lift Pervasives.succ;;
let pred = lift Pervasives.pred;;

let max_nat = unsafe_of_int Pervasives.max_int;;
let min_nat = _0_;;

let of_float f = of_int (Pervasives.int_of_float f);;
let to_float n = Pervasives.float_of_int (to_int n);;

let of_string s = of_int (Pervasives.int_of_string s);;
let to_string s = Pervasives.string_of_int (to_int s);;

let compare x y = Pervasives.( - ) (to_int x) (to_int y);;

let ( // ) a b =
  let q = div a b in
  if (* a * b <= 0 || *) a = mul b q then q else succ q
;;

let split n k =
  let rec loop accu s nn kk =
    if s = k then accu else
    if nn = _0_ then loop (_0_ :: accu) (succ s) nn kk else
    (* It is not necessary to test the nullity of kk, *)
    (* since kk = 0 && nn != 0 is impossible! *)
    (* Indeed, kk is positive and decreases by one at each loop step, *)
    (* and as soon as kk = 1, then q = nn // 1 = nn and we loop with nn = 0. *)
    let q = nn // kk in
    loop (q :: accu) (succ s) (sub nn q) (pred kk) in
  loop [] _0_ n k
;;

let psplit n k = List.filter (fun i -> i <> _0_) (split n k);;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
