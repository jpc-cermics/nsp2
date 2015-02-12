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

(* val get_include_code : string -> string -> string -> string *)

val write_header_file :
  Stringarg.object_rec -> bool -> string -> (string, string) Hashtbl.t -> unit

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)

