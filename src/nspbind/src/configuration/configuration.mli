(***********************************************************************)
(*                                                                     *)
(*                               Interface generator                   *)
(*                                                                     *)
(*                   Pierre Weis, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2010-2015,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)


type error =
   | Fatal_error of string
;;

exception Error of error;;

val report_error: Format.formatter -> error -> unit
;;

val print_version: Codegen_configuration.version -> unit;;

val set_verbose: unit -> unit;;
val get_verbose: unit -> bool;;

val set_debug: unit -> unit;;
val get_debug: unit -> bool;;

val set_no_warnings: unit -> unit;;
val get_no_warnings: unit -> bool;;

val set_warnings: unit -> unit;;
val get_warnings: unit -> bool;;

val set_debug_parsing: unit -> unit;;

val get_definitions_source_file: unit -> Path.file_name;;
val get_overrides_source_file: unit -> Path.file_name;;
val get_target_file: unit -> Path.file_name;;

val is_definitions_source_file_name: Path.file_name -> bool;;
val is_overrides_source_file_name: Path.file_name -> bool;;

val get_software_name: unit -> string;;
val get_software_version: unit -> Codegen_configuration.version;;

(** Internal usage. *)

val definitions_source_file_extension: Path.file_extension;;
val overrides_source_file_extension: Path.file_extension;;

val set_definitions_source_file: Path.file_name -> unit;;
val set_source_file_basename: Path.file_name -> unit;;
val set_overrides_source_file: Path.file_name -> unit;;
val set_target_file: Path.file_name -> unit;;

val set_prefix: string -> unit;;
val get_prefix: unit -> string;;
val default_prefix: unit -> string option;;
val set_prefix_from_object: string -> unit;;

val get_source_file_basename: unit -> Path.file_name;;

val set_path_to_override_for_c: Path.file_name -> unit;;
val set_path_to_override_for_h: Path.file_name -> unit;;

val get_path_to_override_for_c: unit -> Path.file_name;;
val get_path_to_override_for_h: unit -> Path.file_name;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
