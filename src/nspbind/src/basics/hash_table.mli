(***********************************************************************)
(*                                                                     *)
(*                           Simport                                   *)
(*                                                                     *)
(*          Pierre Weis, INRIA Rocquencourt                            *)
(*                                                                     *)
(*  Copyright 2010-2013,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type ('a, 'b) t = ('a, 'b) Hashtbl.t;;

val create : Unatural_types.natural -> ('a, 'b) t;;
val bindings : ('a, 'b) t -> ('a * 'b) list;;
val values : ('a, 'b) t -> 'b list;;
val of_bindings : ('a * 'b) list -> ('a, 'b) t;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
