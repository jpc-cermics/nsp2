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

val name_search : string -> string -> string -> string
val write_getset_set_names :
  Stringarg.object_rec ->
  string -> string -> string -> string * string * string * string
val get_field_accessor_def :
  Stringarg.object_rec -> string -> string -> string
val get_field_accessor_boxed : Stringarg.object_rec -> string -> 'a -> string
val get_field_accessor_gobject :
  Stringarg.object_rec -> string -> string -> string
val get_field_accessor_pointer :
  Stringarg.object_rec -> string -> 'a -> string
val get_field_accessor : Stringarg.object_rec -> string -> string -> string
val write_getter :
  Stringarg.object_rec ->
  bool ->
  string ->
  string ->
  string ->
  Stringarg.function_params ->
  Stringarg.info -> Stringarg.wrapper_info * string
val write_getter_obj :
  Stringarg.object_rec ->
  string -> string -> string -> Stringarg.wrapper_info -> string -> string
val write_setter :
  Stringarg.object_rec ->
  string -> string -> string -> Stringarg.function_params -> string
val write_getset :
  Stringarg.object_rec ->
  bool ->
  string -> string -> Stringarg.function_params -> string list -> string list
val write_getsets : Stringarg.object_rec -> bool -> unit


(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
