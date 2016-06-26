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

val get_override_pattern : string -> string -> string
val type_tmpl_top1 : string
val type_tmpl_top2 : string
val type_tmpl_top3 : string
val insert_loop_code : string -> (string, string) Hashtbl.t -> unit
val insert_path_extract : string -> 'a -> unit
val insert_methods : string -> 'a -> (string, string) Hashtbl.t -> unit
val insert_methods_gtk : 'a -> 'b -> (string, string) Hashtbl.t -> unit
val type_tmpl_gtk_0 : string
val type_tmpl_0 : string
val type_tmpl_1 : string -> string
val type_tmpl_2 : string -> string -> string
val type_tmpl_4 : bool -> string
val type_tmpl_implements : string
val insert_implements :
  string -> Stringarg.object_rec -> (string, string) Hashtbl.t -> unit
val insert_type :
  string ->
  Stringarg.object_rec -> bool -> (string, string) Hashtbl.t -> unit
val type_tmpl_init : string -> string
val insert_init :
  'a -> Stringarg.object_rec -> (string, string) Hashtbl.t -> unit
val type_tmpl_new : string
val insert_new : 'a -> 'b -> (string, string) Hashtbl.t -> unit
val type_tmpl_size : string
val insert_size : string -> (string, string) Hashtbl.t -> unit
val type_tmpl_type_as_string : string -> string
val insert_type_as_string : string -> bool -> (string, string) Hashtbl.t -> unit
val type_tmpl_equalities : string -> string
val insert_equal :
  string -> Stringarg.object_rec -> (string, string) Hashtbl.t -> unit
val type_tmpl_save_load : string -> string -> string -> string
val insert_save_load :
  string -> Stringarg.object_rec -> (string, string) Hashtbl.t -> unit
val type_tmpl_delete : string -> string -> string -> string
val insert_delete :
  string -> Stringarg.object_rec -> (string, string) Hashtbl.t -> unit
val type_tmpl_info : string
val insert_info : string -> (string, string) Hashtbl.t -> unit
val type_tmpl_print : string -> string -> string -> string
val insert_print :
  string -> Stringarg.object_rec -> (string, string) Hashtbl.t -> unit
val type_tmpl_latex : string -> string
val insert_latex : Stringarg.object_rec -> (string, string) Hashtbl.t -> unit
val type_tmpl_interface_util : string -> string
val insert_interface_util : bool -> (string, string) Hashtbl.t -> unit
val type_tmpl_create_header : string
val type_tmpl_create :
  string -> string -> string -> string -> string -> string -> string
val insert_create :
  string -> Stringarg.object_rec -> (string, string) Hashtbl.t -> unit
val type_tmpl_copy : string -> string -> string -> string -> string -> string -> string
val insert_copy :
  string -> Stringarg.object_rec -> (string, string) Hashtbl.t -> unit
val type_tmpl_copy_gtk : string
val type_tmpl_copy_gtk_boxed : string
val type_tmpl_copy_gtk_pointer : string
val insert_copy_gtk :
  'a -> Stringarg.object_rec -> (string, string) Hashtbl.t -> unit
val type_tmpl_intcreate : string -> string -> string
val insert_int_create :
  string -> Stringarg.object_rec -> (string, string) Hashtbl.t -> unit
val write_slots : string -> string list -> unit
val hash_check_slot :
  Stringarg.object_rec ->
  bool -> (string, string) Hashtbl.t -> string -> unit
val get_initial_class_substdict :
  Stringarg.object_rec -> (string, string) Hashtbl.t
val write_class :
  Stringarg.object_rec ->
  (string, string) Hashtbl.t -> Stringarg.function_obj list
val insert_headers : unit -> unit
val insert_enum_value : Stringarg.e_field -> unit
val insert_enum : Stringarg.enum -> unit
val write_enums : Stringarg.parser -> string -> 'a -> unit
val type_tmpl_copyright : string
val write_source : Format.formatter -> Stringarg.parser -> string -> unit

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
