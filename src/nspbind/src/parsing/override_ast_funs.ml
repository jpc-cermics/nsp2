(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2019,                                               *)
(*  Ecole Nationale des ponts et chauss�es                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Ast_node_utils;;

(* val mk_dummy_ast_node :
   'a -> ('a, Ast_node.ast_node_info) Ast_node.ast_node;; *)

let mk_dummy_ast_node desc =
  Ast_node_utils.make_dummy_ast_node desc Override_ast.ParsingMtlb
;;

(** val mk : 'a -> ('a, Ast.ast_node_info) Ast_node.ast_node;; *)

let mk desc =
  let node = Ast_node_utils.make_ast_node (get_symbol_loc ()) desc Override_ast.ParsingMtlb in 
  (* Printf.printf "XXXX %d\n" (Override_location.get_line node.Ast_node.ast_loc); *)
  node 
;;

let change_ast_desc node desc =
  Ast_node_utils.change_ast_node_contents node desc Override_ast.ParsingMtlb
;;

(*
 Local Variables:
  compile-command: "cd ../..make"
  End:
*)
