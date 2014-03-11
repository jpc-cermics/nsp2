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

open Simport_configuration;;

type error =
   | Fatal_error of string
;;

exception Error of error;;

val report_error : Format.formatter -> error -> unit
;;

val print_version : Simport_configuration.version -> unit;;
val print_interface_version : Simport_configuration.interface_version -> unit;;

val get_target_language : unit -> Simport_configuration.target_language;;
val set_target_language : string -> unit;;

val get_source_language : unit -> Simport_configuration.source_language;;
val set_source_language : string -> unit;;

val translator_of_source_file_extension :
  Path.file_extension -> Simport_configuration.translator option
;;

val get_translator : unit -> Simport_configuration.translator;;
val set_translator : string -> unit;;

val get_interfaces : unit -> Ident.ident list;;
val add_interface : Ident.ident -> unit;;

val add_simulink_pervasives_interface : unit -> unit;;

val set_verbose : unit -> unit;;
val get_verbose : unit -> bool;;

val set_debug : unit -> unit;;
val get_debug : unit -> bool;;

val set_debug_slx : unit -> unit;;
val get_debug_slx : unit -> bool;;

val set_no_warnings : unit -> unit;;
val get_no_warnings : unit -> bool;;

val set_warnings : unit -> unit;;
val get_warnings : unit -> bool;;

val set_debug_parsing : unit -> unit;;


val set_source_file : Path.file_name -> unit;;
val get_source_file : unit -> Path.file_name;;

val set_companion_source_file : Path.file_name -> unit;;
val get_companion_source_file : unit -> Path.file_name;;
val get_companion_source_file_option : unit -> Path.file_name option;;

val set_target_file : Path.file_name -> unit;;
val get_target_file : unit -> Path.file_name;;

val set_companion_target_file : Path.file_name -> unit;;
val get_companion_target_file : unit -> Path.file_name;;

val simulink_interface_marshaled_file_name :
  Path.explicit_file_name -> Simport_configuration.interface_marshaled_file_name
;;

val simulink_implementation_marshaled_file_name :
  Path.explicit_file_name ->
  Simport_configuration.implementation_marshaled_file_name
;;
val simulink_implementation_translated_file_name :
  Simport_configuration.implementation_source_file_name ->
  Simport_configuration.implementation_translated_file_name
;;

val matlab_translated_file_name :
  Simport_configuration.matlab_source_file_name ->
  Simport_configuration.matlab_translated_file_name
;;

val mtlb_translated_file_name :
  Simport_configuration.mtlb_source_file_name ->
  Simport_configuration.mtlb_translated_file_name
;;

val sime_translated_file_name :
  Simport_configuration.sime_source_file_name ->
  Simport_configuration.sime_translated_file_name
;;

val is_simulink_interface_source_file_name : Path.file_name -> bool;;
val is_mdl_implementation_source_file_name : Path.file_name -> bool;;
val is_xml_implementation_source_file_name : Path.file_name -> bool;;
val is_slx_implementation_source_file_name : Path.file_name -> bool;;
val is_mdl_implementation_source_explicit_file_name :
  Path.explicit_file_name -> bool;;
val is_xml_implementation_source_explicit_file_name :
  Path.explicit_file_name -> bool;;
val is_slx_implementation_source_explicit_file_name :
  Path.explicit_file_name -> bool;;
val is_simulink_implementation_source_file_name : Path.file_name -> bool;;
val is_simulink_companion_source_file_name : Path.file_name -> bool;;
val is_matlab_source_file_name : Path.file_name -> bool;;
val is_mtlb_source_file_name : Path.file_name -> bool;;
val is_sime_source_file_name : Path.file_name -> bool;;

val is_simulink_translator_source_file_name : Path.file_name -> bool;;
val is_mtlb_translator_source_file_name : Path.file_name -> bool;;

val is_simulink_implementation_marshaled_file_name :
  Path.file_name -> bool;;
val is_simulink_interface_marshaled_file_name : Path.file_name -> bool;;

val set_default_font : Simport_configuration.font -> unit;;
val get_default_font : unit -> font;;

val get_software_name : unit -> string;;
val get_software_version : unit -> Simport_configuration.version;;
val get_interface_version : unit -> Simport_configuration.interface_version;;

val get_search_path : unit -> Path.search_path;;
val set_search_path : Path.search_path -> unit;;

val get_window_width : unit -> Simport_configuration.natural;;
val get_window_height : unit -> natural;;

val get_diagram_left_margin : unit -> natural;;
val get_diagram_right_margin : unit -> natural;;
val get_diagram_bottom_margin : unit -> natural;;
val get_block_vertical_spacing : unit -> natural;;

val set_block_port_width : natural -> unit;;
val set_block_port_height : natural -> unit;;
val set_block_geometry_magnification : natural -> unit;;
val get_block_port_width : unit -> natural;;
val get_block_port_height : unit -> natural;;
val get_block_geometry_magnification : unit -> natural;;

val get_not_yet_translated_mark : unit -> string;;


(** Internal usage. *)
val get_mean_blocks_by_system : unit -> natural;;
val get_mean_block_bindings : unit -> natural;;
val get_max_block_types : unit -> natural;;
val get_mean_block_parameters : unit -> natural;;

val simulink_interface_source_file_extension : Path.file_extension;;
val mdl_implementation_source_file_extension : Path.file_extension;;
val xml_implementation_source_file_extension : Path.file_extension;;
val slx_implementation_source_file_extension : Path.file_extension;;
val simulink_companion_source_file_extension : Path.file_extension;;

val matlab_source_file_extension : Path.file_extension
;;
val mtlb_source_file_extension : Path.file_extension
;;
val sime_source_file_extension : Path.file_extension
;;

val simulink_interface_marshaled_file_extension : Path.file_extension
;;
val simulink_implementation_marshaled_file_extension : Path.file_extension
;;

val source_file_extension_of_source_language :
  Simport_configuration.source_language -> Path.file_extension
;;

(* val string_of_language : Simport_configuration.language -> string *)
(* ;; *)

val init_and_check : unit -> unit;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
