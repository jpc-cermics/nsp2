# 17 "src/matlab/parsing/lisp_lexer.mll"
 

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
    let loc = Mtlb_location.mk_loc sp ep in
    Format.fprintf ppf
      "%a@.Lexical error: %a@."
      Mtlb_location.print loc
      report_error r
  | exn -> raise exn
;;


# 51 "src/matlab/parsing/lisp_lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\246\255\021\000\002\000\090\000\001\000\252\255\003\000\
    \254\255\255\255\253\255\249\255\247\255\248\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\005\000\004\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\255\255\000\000\255\255\003\000\255\255\005\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\009\000\009\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \009\000\000\000\003\000\002\000\011\000\000\000\000\000\007\000\
    \008\000\006\000\004\000\010\000\000\000\004\000\000\000\000\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\000\000\005\000\000\000\000\000\000\000\000\000\
    \000\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\012\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\004\000\000\000\000\000\004\000\
    \000\000\013\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\255\255\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\005\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\000\000\003\000\255\255\255\255\000\000\
    \000\000\000\000\000\000\007\000\255\255\000\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\002\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\004\000\255\255\255\255\004\000\
    \255\255\002\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\005\000\003\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 69 "src/matlab/parsing/lisp_lexer.mll"
                    ( token lexbuf )
# 173 "src/matlab/parsing/lisp_lexer.ml"

  | 1 ->
# 70 "src/matlab/parsing/lisp_lexer.mll"
        ( LPAREN )
# 178 "src/matlab/parsing/lisp_lexer.ml"

  | 2 ->
# 71 "src/matlab/parsing/lisp_lexer.mll"
             ( LPAREN )
# 183 "src/matlab/parsing/lisp_lexer.ml"

  | 3 ->
# 72 "src/matlab/parsing/lisp_lexer.mll"
        ( RPAREN )
# 188 "src/matlab/parsing/lisp_lexer.ml"

  | 4 ->
# 73 "src/matlab/parsing/lisp_lexer.mll"
                  ( token lexbuf )
# 193 "src/matlab/parsing/lisp_lexer.ml"

  | 5 ->
# 74 "src/matlab/parsing/lisp_lexer.mll"
                                    ( NAME(Lexing.lexeme lexbuf) )
# 198 "src/matlab/parsing/lisp_lexer.ml"

  | 6 ->
let
# 75 "src/matlab/parsing/lisp_lexer.mll"
                        s
# 204 "src/matlab/parsing/lisp_lexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) (lexbuf.Lexing.lex_curr_pos + -1) in
# 75 "src/matlab/parsing/lisp_lexer.mll"
                                ( NAME(s) )
# 208 "src/matlab/parsing/lisp_lexer.ml"

  | 7 ->
# 76 "src/matlab/parsing/lisp_lexer.mll"
            ( NAME("t"))
# 213 "src/matlab/parsing/lisp_lexer.ml"

  | 8 ->
# 77 "src/matlab/parsing/lisp_lexer.mll"
            ( NAME("f"))
# 218 "src/matlab/parsing/lisp_lexer.ml"

  | 9 ->
# 78 "src/matlab/parsing/lisp_lexer.mll"
        ( EOF )
# 223 "src/matlab/parsing/lisp_lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

# 80 "src/matlab/parsing/lisp_lexer.mll"
 

(* zone for extra functions *)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)


# 241 "src/matlab/parsing/lisp_lexer.ml"