(***********************************************************************)
(*                                                                     *)
(*          Interface generator                                        *)
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

val write_methods: Stringarg.object_rec -> bool -> unit;;
val write_constructors:  Stringarg.object_rec -> bool -> 
  (string, string) Hashtbl.t -> Stringarg.function_obj list;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
