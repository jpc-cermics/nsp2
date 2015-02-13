
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

open Override_ast;;
open Override_ast_funs;;

(* *)


%}

%token EOF
%token <string> NAME
%token <string>  IGNORE
%token <string * string >  RULE2
%token <string * string * string >  RULE3
%token <string * string * string * string >  RULE4

// Where to start.

%start implementation_file
%type <Override_ast.parsing Override_ast.implementation_file> implementation_file

%%

implementation_file: 
  | overrides  { mk (Ast(List.rev $1)) }

overrides: 
  | rule               { ($1)::[]; }
  | overrides rule     { ($2)::$1; }

rule:
  | IGNORE names { mk ( Ignore($2))}
  | RULE2 { let (a,b) = $1 in mk (Override2(a,b))} 
  | RULE3 { let (a,b,c) = $1 in mk (Override3(a,b,c))} 
  | RULE4 { let (a,b,c,d) = $1 in mk (Override4(a,b,c,d))} 

names:
  |  { []}
  | NAME names { $1 :: $2 }


/*
 Local Variables:
  compile-command: "cd ../../; make"
  End:
*/



