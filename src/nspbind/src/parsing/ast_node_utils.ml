(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2014,                                               *)
(*  Ecole Nationale des ponts et chaussées                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** {6 General exception mechanism for functions walking on ast nodes} *)

type execution_error = {
  error : exn; begin_pos : Lexing.position; end_pos : Lexing.position;
}
;;

exception Execution_error of execution_error
;;

let execution_error exc ast =
  let begin_pos = ast.Ast_node.ast_loc.Override_location.loc_beg
  and end_pos = ast.Ast_node.ast_loc.Override_location.loc_end in
  raise
    (Execution_error {
       error = exc; begin_pos = begin_pos; end_pos = end_pos;
     })
;;

(** {6 Finding locations} *)

let get_symbol_loc () =
  Override_location.mk_loc
    (Parsing.symbol_start_pos ())
    (Parsing.symbol_end_pos ())
;;

let get_rhs_loc n =
  Override_location.mk_loc
    (Parsing.rhs_start_pos n)
    (Parsing.rhs_end_pos n)
;;

let get_unary_op_loc () = get_rhs_loc 1
;;
let get_binary_op_loc () = get_rhs_loc 2
;;
let get_op_loc = get_rhs_loc
;;

(** {6 Bulding ast nodes} *)

let make_ast_node loc desc info = {
  Ast_node.ast_loc = loc;
  ast_desc = desc;
  ast_info = info;
}
;;

let make_dummy_ast_node desc = make_ast_node Override_location.none desc;;

let change_ast_node_desc ast_node desc = {
  ast_node with
  Ast_node.ast_desc = desc;
}
;;

let change_ast_node_contents ast_node desc info =
  {
    ast_node with
    Ast_node.ast_desc = desc;
    Ast_node.ast_info = info;
  }
;;

let change_ast_node_info ast_node forget =
  {
    ast_node with
    Ast_node.ast_info = forget;
  }
;;

let forget_ast_node_info forget ast_node =
  change_ast_node_info ast_node forget
;;

(*
val lift_fun_info :
  ('info -> 'a) ->
  ('desc, 'info) Ast_node.ast_node ->
  ('desc, 'a) Ast_node.ast_node
;;
*)
(*
let lift_fun_info f_info ast =
  let info = f_info ast.Ast_node.info in
  change_info ast info
;;
*)

let lift_fun_contents f_contents ast =
  let desc, info = f_contents ast.Ast_node.ast_desc in
  change_ast_node_contents ast desc info
;;

let lift_fun_contents2 f_contents env ast =
  let env, (desc, info) = f_contents env ast.Ast_node.ast_desc in
  env, change_ast_node_contents ast desc info
;;

let lift_fun_desc info f_desc =
  let f_contents desc = f_desc desc, info in
  lift_fun_contents f_contents
;;

let lift_fun_desc2 info f_desc =
  let f_contents env desc =
    let env, desc = f_desc env desc in
    env, (desc, info) in
  lift_fun_contents2 f_contents
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
