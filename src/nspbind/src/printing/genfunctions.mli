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

val check_matchers:  Stringarg.function_params list ->
  string -> bool * string
val write_functions: (string, string) Hashtbl.t -> unit;;
val write_function_table:  Stringarg.function_obj list ->
  bool -> (string, 'a) Hashtbl.t -> string;;
val write_function_wrapper:  string ->  bool ->  Stringarg.function_obj ->
  string -> string -> bool -> bool -> (string, string) Hashtbl.t  -> string * string
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
