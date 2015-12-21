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

val bb : Buffer.t
val build_copy_partial : Stringarg.object_rec -> 'a -> string -> string
val build_copy_partial_parent : Stringarg.object_rec -> 'a -> string -> string
val build_fields : Stringarg.object_rec -> string
val build_fields_ref : Stringarg.object_rec -> string
val build_copy_fields :
  Stringarg.object_rec ->
  Stringarg.left_varname ->
  Stringarg.right_varname -> Stringarg.f_copy_name -> string
val build_copy_fields_default : Stringarg.object_rec -> 'a -> string
val build_fields_full_copy_partial_code :
  Stringarg.object_rec ->
  'a -> string -> string -> Stringarg.f_copy_name -> string
val build_save_fields : Stringarg.object_rec -> Stringarg.varname -> string
val build_info_fields : Stringarg.object_rec -> Stringarg.varname -> string
val build_print_fields :
  Stringarg.object_rec -> Stringarg.varname -> Stringarg.print_mode -> string
val build_init_fields : Stringarg.object_rec -> Stringarg.varname -> string
val build_load_fields : Stringarg.object_rec -> Stringarg.varname -> string
val build_fields_from_attributes : Stringarg.object_rec -> 'a -> string
val build_defval_fields : Stringarg.object_rec -> Stringarg.varname -> string
val build_fields_free1 : Stringarg.object_rec -> string -> string
val build_fields_free2 : Stringarg.object_rec -> Stringarg.varname -> string
val build_equal_fields : Stringarg.object_rec -> Stringarg.varname -> string
val build_full_copy_code : Stringarg.object_rec -> 'a -> 'b -> string
val build_list_fields : Stringarg.object_rec -> 'a -> string
val build_create_partial :
  Stringarg.object_rec -> Stringarg.varname -> string
val build_interfaces : Stringarg.object_rec -> string

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
