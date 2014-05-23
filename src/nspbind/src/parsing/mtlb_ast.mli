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

open Ast_node;;

type parsing =
   | ParsingMtlb
;;

(* Abstract syntax tree produced by parsing *)

type 'info implementation_file = ('info implementation_file_desc, 'info) ast_node
and  'info implementation_file_desc =
  | Ast of 'info override list

and 'info override =  ('info override_desc, 'info ) ast_node

and 'info override_desc =
  | Ignore of string list
  | Override2 of string * string
  | Override3 of string * string * string
  | Override4 of string * string * string  * string 

(*
 Local Variables:
  compile-command: "cd ../../..; make"
  End:
*)
