(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          Pierre Weis, INRIA Rocquencourt                            *)
(*                                                                     *)
(*  Copyright 2010-2019,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Lexing;;

type file_name = string;;

type position = Lexing.position = {
  pos_fname : file_name;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
;;

(** The location of an AST node,
    beginning and ending position of its corresponding source text. *)
type t = {
  loc_start : position;
  loc_end : position;
}
;;

let set_lexbuf_filename_positions fname lexbuf =
  lexbuf.Lexing.lex_start_p <-
    { lexbuf.lex_start_p
      with Lexing.pos_fname = fname; };
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.lex_curr_p
      with pos_fname = fname; }
;;

let lexing_from_file fname =
  let ic = Stdlib.open_in fname in
  let lexbuf = Lexing.from_channel ic in
  set_lexbuf_filename_positions fname lexbuf;
  ic, lexbuf
;;

let symbol_rloc () = {
  loc_start = Parsing.symbol_start_pos ();
  loc_end = Parsing.symbol_end_pos ();
};;

let none =
  let pos_none =
    { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 } in
  { loc_start = pos_none; loc_end = pos_none }
;;

let print_token_location ppf (start_pos, end_pos) =
  if start_pos.pos_lnum = end_pos.pos_lnum then
    (* Starts and ends on a same line. *)
    Format.fprintf ppf
      "line %d, characters %d-%d"
      start_pos.pos_lnum
      (start_pos.pos_cnum - start_pos.pos_bol)
      (end_pos.pos_cnum - end_pos.pos_bol)
  else
    Format.fprintf ppf
      "line %d, character %d, line %d, character %d"
      start_pos.pos_lnum
      (start_pos.pos_cnum - start_pos.pos_bol)
      end_pos.pos_lnum
      (end_pos.pos_cnum - end_pos.pos_bol)
(*
  Format.fprintf ppf
    "line %d, characters %d-%d"
    start_pos.Lexing.pos_lnum
    (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)
    (* end_pos.pos_cnum - start_pos.pos_bol:
       we do not write end_pos.pos_cnum - end_pos.pos_bol because
       if the error spreads on more than one line, we must report the entire
       error location *)
    (end_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)
*)
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
