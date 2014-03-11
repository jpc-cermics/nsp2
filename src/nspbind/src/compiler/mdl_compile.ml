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

let read_simulink_implementation_marshaled_file,
    write_simulink_implementation_marshaled_file =
  Umarshal.read_write_file ()
;;

(** From Simulink interface file to compiled interface file. *)
let compile_simulink_interface _efname =
  failwith (Format.sprintf "Not implemented")
;;

(** From Simulink implementation file to compiled implementation file. *)
let write_translated_source_file _dst_efname _code =
  failwith (Format.sprintf "Not implemented")
;;

(** From Simulink compiled implementation file to scicos host source file. *)
let translate_simulink_implementation_marshaled_file src_efname =
  let code = read_simulink_implementation_marshaled_file src_efname in
  let dst_efname =
    Configuration.simulink_implementation_translated_file_name src_efname in
  write_translated_source_file dst_efname code
;;

(*
let not_implemented s =
  failwith (Format.sprintf "Mdl_compile.%s not implemented" s)
;;
*)

let read_mdl_ast _src_efname =
  failwith (Format.sprintf "Not implemented")
;;

let read_xml_ast _src_efname =
  failwith (Format.sprintf "Not implemented")
;;

let read_slx_ast _src_efname =
  failwith (Format.sprintf "Not implemented")
;;

let read_simulink_ast src_efname =
  if Configuration.is_mdl_implementation_source_explicit_file_name src_efname
    then read_mdl_ast src_efname else
  if Configuration.is_xml_implementation_source_explicit_file_name src_efname
    then read_xml_ast src_efname else
  if Configuration.is_slx_implementation_source_explicit_file_name src_efname
    then read_slx_ast src_efname else
  assert false
;;

let compile_simulink_implementation _src_efname =
  failwith (Format.sprintf "Not implemented")
;;

let write_companion_translated_source_file dst_efname src_efname =
  Path.with_explicit_out_file dst_efname (fun oc ->
    let ppf = Format.formatter_of_out_channel oc in
    Format.fprintf ppf "%a@."
      Override_print.translate_file src_efname)
;;

let translate_matlab_source_file src_efname =
  let dst_efname = Configuration.matlab_translated_file_name src_efname in
  write_companion_translated_source_file dst_efname src_efname
;;

let translate_mtlb_source_file src_efname =
  let dst_efname = Configuration.mtlb_translated_file_name src_efname in
  write_companion_translated_source_file dst_efname src_efname
;;

let write_sime_translated_source_file _dst_efname _src_efname =
  failwith (Format.sprintf "Not implemented")
;;

let translate_sime_source_file src_efname =
  let dst_efname = Configuration.sime_translated_file_name src_efname in
  write_sime_translated_source_file dst_efname src_efname
;;

let compile src_fname =
  let src_efname = Path.find src_fname in

  if Configuration.is_simulink_interface_source_file_name src_fname
    then compile_simulink_interface src_efname else
  if Configuration.is_simulink_implementation_source_file_name src_fname
    then compile_simulink_implementation src_efname else
  if Configuration.is_simulink_implementation_marshaled_file_name src_fname
    then translate_simulink_implementation_marshaled_file src_efname else

  if Configuration.is_mtlb_source_file_name src_fname
    then translate_mtlb_source_file src_efname else
  if Configuration.is_sime_source_file_name src_fname
    then translate_sime_source_file src_efname else

  if Configuration.is_matlab_source_file_name src_fname
    then translate_matlab_source_file src_efname else

  Say.fatal_error
    (Printf.sprintf "do not know what to do with file %S" src_fname)
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
