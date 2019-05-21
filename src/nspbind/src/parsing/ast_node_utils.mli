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

(** {6 Handling and raising errors } *)

type execution_error = {
  error : exn;
  begin_pos : Lexing.position;
  end_pos : Lexing.position;
}
;;

exception Execution_error of execution_error;;

val execution_error : exn -> ('a, 'b) Ast_node.ast_node -> 'c;;

(* {6 Finding AST locations } *)

val get_symbol_loc : unit -> Override_location.t;;
(* Get the location of the string that matches a grammar rule. *)

val get_rhs_loc : int -> Override_location.t;;
(* Get the location of the string that matches the nth item of
    a grammar rule. *)

val get_unary_op_loc : unit -> Override_location.t;;
val get_binary_op_loc : unit -> Override_location.t;;
val get_op_loc : int -> Override_location.t;;
(* [get_unary_op_loc ()] get the location of the string that matches a unary
 prefix operator of a grammar rule.
 [get_binary_op_loc ()] get the location of the string that matches a binary
 infix operator of a grammar rule.
 [get_op_loc n] get the location of the string that matches a mixfix
 operator at position [n] of a grammar rule.
 (Hence, [get_op_loc] is an alias for [get_rhs_loc].)
*)

(* {6 Building AST nodes } *)

val make_ast_node :
  Override_location.t -> 'desc -> 'info -> ('desc, 'info) Ast_node.ast_node
;;
(* [make_ast_node loc desc info] build an [ast_node] with the corresponding
  fields. *)

val make_dummy_ast_node :
  'desc -> 'info -> ('desc, 'info) Ast_node.ast_node
;;
(* [make_dummy_ast_node desc info] build an [ast_node] with the corresponding
  fields and a dummy [Override_location.t]. *)

val change_ast_node_contents :
  ('a, 'b) Ast_node.ast_node ->
  'desc -> 'info -> ('desc, 'info) Ast_node.ast_node
;;

val change_ast_node_desc :
  ('a, 'b) Ast_node.ast_node ->
  'desc -> ('desc, 'b) Ast_node.ast_node
;;

val forget_ast_node_info :
  'forget ->
  ('a, 'info) Ast_node.ast_node ->
  ('a, 'forget) Ast_node.ast_node
;;

val change_ast_node_info :
  ('a, 'b) Ast_node.ast_node ->
  'info -> ('a, 'info) Ast_node.ast_node
;;

val lift_fun_contents :
  ('a -> ('desc * 'info)) ->
  ('a, 'b) Ast_node.ast_node ->
  ('desc, 'info) Ast_node.ast_node
;;
(* [lift_fun_contents f_contents ast]
    given a function mapping a node descriptor to a new descriptor and info,
    returns a node with the new contents and the same location as [ast]. *)

val lift_fun_contents2 :
  ('env -> 'a -> 'env * ('desc * 'info)) ->
  'env ->
  ('a, 'b) Ast_node.ast_node ->
  'env * ('desc, 'info) Ast_node.ast_node
;;
(* [lift_fun_contents2 f_contents ast]
  given a function mapping a node descriptor to a new descriptor and info
  while accumulating into an environment, returns the new environment and a
  node with the new contents and the same location as [ast]. *)

val lift_fun_desc :
  'info ->
  ('desc -> 'a) ->
  ('desc, 'b) Ast_node.ast_node ->
  ('a, 'info) Ast_node.ast_node
;;
(* [lift_fun_desc info f_desc ast]
  given an info value and a function mapping a node descriptor to a new
  descriptor, returns a node with the new contents and the same location as
  [ast]. *)

val lift_fun_desc2 :
  'info ->
  ('env -> 'desc -> 'env * 'a) ->
  'env ->
  ('desc, 'b) Ast_node.ast_node ->
  'env * ('a, 'info) Ast_node.ast_node
;;
(* [lift_fun_desc2 info f_contents ast] given an [info] value and a function
  mapping a node descriptor to a new descriptor while accumulating into an
  environment, returns the new environment and a node with the new descriptor
  and info, and the same location as [ast]. *)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
