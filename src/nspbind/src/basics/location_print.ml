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

open Location;;

(** The location of an AST node,
    beginning and ending position of its corresponding source text. *)

let print_t ppf { loc_start = loc_start; loc_end = loc_end } =
  let file_name = loc_start.pos_fname in
  Format.fprintf ppf "File \"%s\", %a"
    file_name
    Location.print_token_location (loc_start, loc_end)
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
