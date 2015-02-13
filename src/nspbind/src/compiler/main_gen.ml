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

let do_phase phase treat_file ppf fname =
  if Configuration.get_verbose () then
    Format.fprintf Format.err_formatter "%s%,ing file %S@." phase fname;
  try treat_file ppf fname;
      if Configuration.get_verbose () then
        Format.fprintf Format.err_formatter
          "File %S successfully %s%,ed@." fname phase;
  with
  | Configuration.Error err ->
    Configuration.report_error Format.err_formatter err
  | Override_lexer.Error (e, pos_begin, pos_end) ->
    let loc = Override_location.mk_loc pos_begin pos_end in
    Format.fprintf Format.err_formatter
      "Lexical error: %a@.%a@."
      Override_location.print loc
      Override_lexer.report_error e
  | Override_syntaxerr.Error (Override_syntaxerr.Other loc) ->
    Format.fprintf Format.err_formatter
      "Syntax error: %a@."
      Override_location.print loc
      (* Override_syntaxerr.report_error (Override_syntaxerr.Other loc) *)
;;

(*
 Local Variables:
  compile-command: "cd ../../..; make"
  End:
*)
