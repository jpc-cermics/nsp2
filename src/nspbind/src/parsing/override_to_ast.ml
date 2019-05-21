(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2019,                                               *)
(*  Ecole Nationale des ponts et chaussees                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Entry points to the parser *)

let parse lexbuf =
  try
    let ast = Override_parser.implementation_file Override_lexer.token lexbuf in
    Parsing.clear_parser();
    ast
  with
  | Override_lexer.Lexer_error(Override_lexer.Unterminated_comment, _, _) as err -> raise err
  | Override_lexer.Lexer_error(Override_lexer.Unterminated_string, _, _) as err -> raise err
  | Override_lexer.Lexer_error(_, _, _) as err -> raise err
  | Override_syntaxerr.Error _ as err -> raise err
  | Parsing.Parse_error ->
      let loc1 =
        { Override_location.loc_beg = lexbuf.Lexing.lex_start_p;
          Override_location.loc_end = lexbuf.Lexing.lex_curr_p} in
      raise (Override_syntaxerr.Error (Override_syntaxerr.Other loc1))
;;

(* Parse a file *)

let parse_ic fname ic =
  let lexbuf = Lexing.from_channel ic in
  Override_location.init lexbuf fname;
  parse lexbuf
;;

let parse_file efname =
  Path.with_explicit_in_file_bin efname (fun ic ->
    parse_ic (Path.of_explicit_file_name efname) ic)
;;

(* Parse a string *)

let parse_string s =
  let lexbuf = Lexing.from_string s in
  Override_location.init lexbuf (Printf.sprintf "<string: %s>" s);
  parse lexbuf
;;

let read_mtlb_file = parse_file;;
let read_mtlb_string = parse_string;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
