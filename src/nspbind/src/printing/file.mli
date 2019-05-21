(***********************************************************************)
(*                                                                     *)
(*                           Interface generator                       *)
(*                                                                     *)
(*          J.Ph Chancelier, Enpc/Cermics                              *)
(*                                                                     *)
(*  Copyright 2012-2019,                                               *)
(*  Ecole Nationale des ponts et chaussees                             *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type file_output = {
  mutable ppf : Format.formatter;
  mutable filename : string;
  mutable override_c_filename : string;
  mutable override_h_filename : string;
  mutable override_filename : string;
  mutable lineno : int;
}

val file_output : file_output
val set_ppf : Format.formatter -> unit
val set_filenames : string -> string -> string -> unit 
val setline : int -> string -> unit
val setline_override : int -> unit
val resetline : unit -> unit
val count_newlines : string -> int
val write : string -> unit
val pattern_to_code : string -> (string, string) Hashtbl.t -> string
val write_string : string -> unit
val write_pattern : string -> (string, string) Hashtbl.t -> unit
val write_override_pattern :
  string -> string -> (string, string) Hashtbl.t -> unit
val write_override : string -> string -> unit
val get_override_c_file_name : unit -> string
val get_override_h_file_name : unit -> string

val pattern_to_code_with_buffer:  string -> (string, string) Hashtbl.t -> string;;
val write_substitute: string -> unit;;
val write_substitute_pattern: string -> (string, string) Hashtbl.t -> unit;;
val code_with_nl: string -> string;;

val set_c_override: unit -> unit;;
val set_h_override: unit -> unit;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
