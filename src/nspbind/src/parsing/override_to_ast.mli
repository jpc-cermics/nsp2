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

(** Entry points to the parser *)

val read_mtlb_file :
  Path.explicit_file_name -> Override_ast.parsing Override_ast.implementation_file;;
val read_mtlb_string :
  string -> Override_ast.parsing Override_ast.implementation_file;;




(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
