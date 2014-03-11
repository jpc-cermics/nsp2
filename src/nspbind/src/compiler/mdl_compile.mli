(***********************************************************************)
(*                                                                     *)
(*                               Simport                               *)
(*                                                                     *)
(*                   Pierre Weis, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2010-2014,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** From Simulink interface/implementation file to compiled interface file. *)

val translate_simulink_implementation_marshaled_file :
  Path.explicit_file_name -> unit
;;
(** From Simulink implementation (pre)compiled file to target language compiled
  source file. *)

val compile : Path.file_name -> unit
;;
(** From simport compatible input file to (target language) compiled file. *)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
