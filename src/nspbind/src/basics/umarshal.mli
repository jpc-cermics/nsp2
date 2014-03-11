(***********************************************************************)
(*                                                                     *)
(*                               Simport                               *)
(*                                                                     *)
(*                   Pierre Weis, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2010-2013,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** From MDL interface file to compiled interface file. *)

(** Reading/Writing files with marshalled values. *)
val of_to_file : unit ->
  (Path.explicit_file_name -> 'a) * (Path.explicit_file_name -> 'a -> unit)
;;
(** A consistent Marshal file reader/writer generator:
  [ of_to_file () ] returns a pair of functions that read/write values of any
  given type. The type of values read/written is fixed by the first usage of
  one of the functions of the resulting pair of functions. *)

val read_write_file : unit ->
  (Path.explicit_file_name -> 'a) * (Path.explicit_file_name -> 'a -> unit)
;;
(** An alias for [of_to_file]. *)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
