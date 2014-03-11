(***********************************************************************)
(*                                                                     *)
(*                               Simport                               *)
(*                                                                     *)
(*                   Pierre Weis, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2010-2014,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

val main :
  (Format.formatter -> Path.file_name -> unit) ->
  Format.formatter -> unit
;;
(** [Arguments.main do_file ppf] applies the function [do_file] to all
  the file arguments of the command defined via [Arguments.main]. *)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
