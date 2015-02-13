(***********************************************************************)
(*                                                                     *)
(*                               Interface generator                   *)
(*                                                                     *)
(*                   Pierre Weis, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2010-2015,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type message = string;;

val warning : message -> unit;;

val fatal_error : message -> 'a;;

val debug : message -> unit;;

val fdebug : (Format.formatter -> unit) -> unit;;
(** [fdebug f] apply [f] to a formatter only if debug is set. *)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
