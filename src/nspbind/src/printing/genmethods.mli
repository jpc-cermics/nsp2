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

(* 
val method_tmpl_std : string
val method_tmpl_boxed : string
val method_tmpl : Stringarg.object_rec -> string
val constructor_tmpl_std : string
val constructor_tmpl_boxed : string
val constructor_tmpl : Stringarg.object_rec -> string
val written_overrides_table : (string, string) Hashtbl.t
val write_method :
  Stringarg.object_rec ->
  bool -> Stringarg.function_obj -> string -> bool -> bool -> string * bool
val find_methods : Stringarg.object_rec -> Stringarg.function_obj list
val write_methods : Stringarg.object_rec -> bool -> unit
val find_constructors : Stringarg.object_rec -> Stringarg.function_obj list

*)

val write_methods: Stringarg.object_rec -> bool -> unit;;
val write_constructors:  Stringarg.object_rec -> bool -> 
  (string, string) Hashtbl.t -> Stringarg.function_obj list;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
