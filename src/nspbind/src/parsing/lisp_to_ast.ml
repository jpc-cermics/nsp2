(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2014,                                               *)
(*  Ecole Nationale des ponts et chaussees                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Lisp_ast;;

(** Entry points to the parser *)

let parse lexbuf =
  try
    let ast = Lisp_parser.implementation_file Lisp_lexer.token lexbuf in
    Parsing.clear_parser();
    ast
  with _ -> failwith "Failed to parse the lisp file" 
;;

(* Parse a file *)

let parse_ic fname ic =
  let lexbuf = Lexing.from_channel ic in
  Mtlb_location.init lexbuf fname;
  parse lexbuf
;;

let parse_file efname =
  Path.with_explicit_in_file_bin efname (fun ic ->
    parse_ic (Path.of_explicit_file_name efname) ic)
;;

(* detect include directives *) 

let check_include sexp = 
  let get_include_name sexp = 
    match sexp with 
    | Cons { car = Atom name ; cdr = Null;} -> Some(name)
    | Cons _ | Atom _ | Null -> None in 
  match sexp with 
  | Cons { car = Atom "include"; cdr = sexp;} -> 
      ( 
	match get_include_name sexp with 
	| None -> false 
	| Some _file -> true
       )
  | Cons _ | Atom _ | Null -> false
;;

let rec check_includes lisp_ast = 
  match lisp_ast with 
  | [] -> false 
  |  sexp :: sexps -> 
      if check_include sexp then 
	true 
      else
	check_includes sexps;
;;

let expand_include sexp = 
  let get_include_name sexp = 
    match sexp with 
    | Cons { car = Atom name ; cdr = Null;} -> Some(name)
    | Cons _ | Atom _ | Null -> None in 
  match sexp with 
  | Cons { car = Atom "include"; cdr = sexp;} -> 
      ( 
	match get_include_name sexp with 
	| None -> [sexp]
	| Some (file) -> 
	    parse_file (Path.find file)
       )
  | Cons _ | Atom _ | Null -> [sexp]
;;

let rec expand_includes lisp_ast accu = 
  match lisp_ast with 
  | [] -> accu 
  |  sexp :: sexps -> 
      (expand_include sexp) @ expand_includes sexps accu
;;

let rec expand_includes_fp lisp_ast = 
  if check_includes lisp_ast then 
    expand_includes_fp (expand_includes lisp_ast [])
  else
    lisp_ast
;;

(* detect register directives *) 

let check_register sexp = 
  let get_register_name sexp = 
    match sexp with 
    | Cons { car = Atom name ; cdr = Null;} -> Some(name)
    | Cons _ | Atom _ | Null -> None in 
  match sexp with 
  | Cons { car = Atom "register"; cdr = sexp;} -> 
      ( 
	match get_register_name sexp with 
	| None -> false 
	| Some _file -> true
       )
  | Cons _ | Atom _ | Null -> false
;;

let rec check_registers lisp_ast = 
  match lisp_ast with 
  | [] -> false 
  |  sexp :: sexps -> 
      if check_register sexp then 
	true 
      else
	check_registers sexps;
;;

let expand_register sexp = 
  let get_register_name sexp = 
    match sexp with 
    | Cons { car = Atom name ; cdr = Null;} -> Some(name)
    | Cons _ | Atom _ | Null -> None in 
  match sexp with 
  | Cons { car = Atom "register"; cdr = sexp;} -> 
      ( 
	match get_register_name sexp with 
	| None -> []
	| Some (file) -> 
	    parse_file (Path.find file)
       )
  | Cons _ | Atom _ | Null -> []
;;

let rec expand_registers lisp_ast accu = 
  match lisp_ast with 
  | [] -> accu 
  |  sexp :: sexps -> 
      (expand_register sexp) @ expand_registers sexps accu
;;

let rec expand_registers_fp lisp_ast = 
  if check_registers lisp_ast then 
    expand_registers_fp (expand_registers lisp_ast [])
  else
    lisp_ast
;;

(* main reader *)

let read_lisp_file efname = 
  let ast = parse_file efname in 
  let ast1 = expand_includes_fp ast in 
  ( ast1 ,   expand_registers_fp ast1)
;;

(*
 Local Variables:
  compile-command: "cd ../../..; make"
  End:
*)
