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

(** Additional functions for module Marshal. *)

(** Reading/Writing files with marshalled values. *)
let of_to_file () =
  (fun fname ->
    Path.with_explicit_in_file_bin fname
      (fun ic -> Marshal.from_channel ic)),
  (fun fname ast ->
    Path.with_explicit_out_file_bin fname
      (fun oc -> Marshal.to_channel oc ast []))
;;

let read_write_file = of_to_file;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
