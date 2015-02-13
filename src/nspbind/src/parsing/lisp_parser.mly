
%{

(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2015,                                               *)
(*  Ecole Nationale des ponts et chaussées                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* *)

%}

%token <string> NAME
%token LPAREN RPAREN EOF
%start implementation_file
%type <Lisp_ast.t list> implementation_file

%%

implementation_file:
  | sexps { $1 }
;
sexps: 
  | sexp { [$1] }
  | sexp sexps { $1 :: $2 }

sexp:
  | list { $1 }
  | atom { $1 }
;

list:
  | LPAREN RPAREN { Lisp_ast.Null }
  | LPAREN inside_list RPAREN { $2 }

inside_list:
  | sexp { Lisp_ast.cons $1 Lisp_ast.Null } 
  | sexp inside_list { Lisp_ast.cons $1 $2 }
;

atom: NAME { Lisp_ast.Atom $1 }
;

/*
 Local Variables:
  compile-command: "cd ../../; make"
  End:
*/



