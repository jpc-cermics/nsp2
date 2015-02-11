(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2013,                                               *)
(*  Ecole Nationale des ponts et chaussées                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

{

(* Prelude part: this is pure Caml. *)

(* open Lisp_ast;; *)
open Lisp_parser;;
(* open Lexing;;*)

(** {6 Lexing errors} *)

type error =
  | Illegal_character of char
  | Unterminated_comment
  | Unterminated_string
  | Test_error
;;

(** The various errors when lexing. *)

exception Error of error * Lexing.position * Lexing.position;;

let error (reason, start_p, curr_p) =
  raise (Error (reason, start_p, curr_p))
;;

(** {6 Explaining lexing errors} *)

let report_error ppf = function
  | Illegal_character c ->
      Format.fprintf ppf "Illegal character %C" c
  | Unterminated_comment ->
      Format.fprintf ppf "Unterminated comment"
  | Unterminated_string ->
      Format.fprintf ppf "Unterminated string"
  | Test_error -> Format.fprintf ppf "Test location\n"
;;

let report_lexical_error ppf = function
  | Error (r, sp, ep) ->
    let loc = Override_location.mk_loc sp ep in
    Format.fprintf ppf
      "%a@.Lexical error: %a@."
      Override_location.print loc
      report_error r
  | exn -> raise exn
;;

}

(* start of lexer rules *)

rule token = parse
    [' ' '\t' '\n'] { token lexbuf }
  | '(' { LPAREN }
  | '\'' '(' { LPAREN }
  | ')' { RPAREN }
  | ';' [^ '\n']* { token lexbuf } (* comments *)
  | ['A'-'z' '0'-'9' '*' '_' '-' ]+ { NAME(Lexing.lexeme lexbuf) }
  | '\"' ( [^ '\"']* as s) '\"' { NAME(s) }
  | '#' 't' { NAME("t")}
  | '#' 'f' { NAME("f")}
  | eof { EOF }

{

(* zone for extra functions *)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)

}
