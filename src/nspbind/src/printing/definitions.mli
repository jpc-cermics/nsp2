(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2015,                                               *)
(*  Ecole Nationale des ponts et chaussees                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

val select_objects: Lisp_ast.t list -> Stringarg.object_rec list;;
val select_interfaces: Lisp_ast.t list -> Stringarg.object_rec list;;
val select_structures: Lisp_ast.t list -> Stringarg.object_rec list;;
val select_pointers: Lisp_ast.t list -> Stringarg.object_rec list;;
val select_boxes: Lisp_ast.t list -> Stringarg.object_rec list;;
val select_functions:  Lisp_ast.t list ->  Stringarg.function_obj list -> Stringarg.function_obj list;;
val select_enums:  Lisp_ast.t list -> Stringarg.enum list -> Stringarg.enum list;;

val print_lisp_ast : Lisp_ast.t list -> unit;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
