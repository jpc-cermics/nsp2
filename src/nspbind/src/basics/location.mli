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

type file_name = string;;

type position = Lexing.position = {
  pos_fname : file_name; (* File name *)
  pos_lnum : int; (* Line number *)
  pos_bol : int; (* Beginning of lexem character position *)
  pos_cnum : int; (* Current lexing character position *)
}
;;

type t = {
 loc_start : position;
 loc_end : position;
}
;;
(** The location of an AST node,
    beginning and ending position of its corresponding source text. *)

val set_lexbuf_filename_positions : file_name -> Lexing.lexbuf -> unit;;
val lexing_from_file : file_name -> Pervasives.in_channel * Lexing.lexbuf;;
(** Open a file and return an input channel and a lexbuf reading from it. *)

val none : t;;

val symbol_rloc : unit -> t;;

val print_token_location : Format.formatter -> (position * position) -> unit;;
(** [print_token_location ppf start_pos end_pos] print the location of the
    characters between [start_pos] (inclusive) and [end_pos] (exclusive). *)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
