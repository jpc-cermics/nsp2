type token =
  | NAME of (string)
  | LPAREN
  | RPAREN
  | EOF

open Parsing;;
let _ = parse_error;;
# 3 "src/parsing/lisp_parser.mly"

(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2019,                                               *)
(*  Ecole Nationale des ponts et chaussées                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* *)

# 28 "src/parsing/lisp_parser.ml"
let yytransl_const = [|
  258 (* LPAREN *);
  259 (* RPAREN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* NAME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\004\000\006\000\
\006\000\005\000\000\000"

let yylen = "\002\000\
\001\000\001\000\002\000\001\000\001\000\002\000\003\000\001\000\
\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\010\000\000\000\011\000\001\000\000\000\004\000\
\005\000\006\000\000\000\000\000\003\000\009\000\007\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\009\000\012\000"

let yysindex = "\005\000\
\003\255\000\000\000\000\000\255\000\000\000\000\003\255\000\000\
\000\000\000\000\003\255\005\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\009\000\000\000\
\000\000\000\000\007\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\004\000\252\255\000\000\000\000\001\000"

let yytablesize = 12
let yytable = "\011\000\
\003\000\004\000\010\000\003\000\004\000\001\000\011\000\015\000\
\002\000\008\000\013\000\014\000"

let yycheck = "\004\000\
\001\001\002\001\003\001\001\001\002\001\001\000\011\000\003\001\
\000\000\003\001\007\000\011\000"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  EOF\000\
  "

let yynames_block = "\
  NAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sexps) in
    Obj.repr(
# 30 "src/parsing/lisp_parser.mly"
          ( _1 )
# 91 "src/parsing/lisp_parser.ml"
               : Lisp_ast.t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sexp) in
    Obj.repr(
# 33 "src/parsing/lisp_parser.mly"
         ( [_1] )
# 98 "src/parsing/lisp_parser.ml"
               : 'sexps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'sexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sexps) in
    Obj.repr(
# 34 "src/parsing/lisp_parser.mly"
               ( _1 :: _2 )
# 106 "src/parsing/lisp_parser.ml"
               : 'sexps))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 37 "src/parsing/lisp_parser.mly"
         ( _1 )
# 113 "src/parsing/lisp_parser.ml"
               : 'sexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'atom) in
    Obj.repr(
# 38 "src/parsing/lisp_parser.mly"
         ( _1 )
# 120 "src/parsing/lisp_parser.ml"
               : 'sexp))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "src/parsing/lisp_parser.mly"
                  ( Lisp_ast.Null )
# 126 "src/parsing/lisp_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'inside_list) in
    Obj.repr(
# 43 "src/parsing/lisp_parser.mly"
                              ( _2 )
# 133 "src/parsing/lisp_parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sexp) in
    Obj.repr(
# 46 "src/parsing/lisp_parser.mly"
         ( Lisp_ast.cons _1 Lisp_ast.Null )
# 140 "src/parsing/lisp_parser.ml"
               : 'inside_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'sexp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'inside_list) in
    Obj.repr(
# 47 "src/parsing/lisp_parser.mly"
                     ( Lisp_ast.cons _1 _2 )
# 148 "src/parsing/lisp_parser.ml"
               : 'inside_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 50 "src/parsing/lisp_parser.mly"
           ( Lisp_ast.Atom _1 )
# 155 "src/parsing/lisp_parser.ml"
               : 'atom))
(* Entry implementation_file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let implementation_file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Lisp_ast.t list)
