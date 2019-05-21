(***********************************************************************)
(*                                                                     *)
(*                               Interface generator                   *)
(*                                                                     *)
(*                   Pierre Weis, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2010-2019,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Codegen_configuration;;

type error =
  | Fatal_error of string
;;

exception Error of error;;

let fatal_error s = raise (Error (Fatal_error s));;

let print_version v = Format.fprintf Format.std_formatter "%s" v;;

let nsp_codegen_name = "nspcodegen";;
let nsp_codegen_version = "0.9.0";;

let definitions_source_file_extension = Path.to_file_extension ".defs"
and overrides_source_file_extension = Path.to_file_extension ".override"
;;

let conf = {
  Codegen_configuration.
  software_name = nsp_codegen_name;
  software_version = nsp_codegen_version;

  verbose = false;
  debug = false;
  warnings = true;

  definitions_source_file = None;
  overrides_source_file = None;
  source_file_basename = None;

  target_file = None;
  prefix = None;

  path_to_override_for_c = None;
  path_to_override_for_h = None;
}
;;

let set_verbose, get_verbose =
  (fun () -> conf.verbose <- true),
  (fun () -> conf.verbose)
;;

let set_debug, get_debug =
  (fun () -> conf.debug <- true),
  (fun () -> conf.debug)
;;

let set_warnings, get_warnings =
  (fun () -> conf.warnings <- true),
  (fun () -> conf.warnings)
;;

let set_no_warnings, get_no_warnings =
  (fun () -> conf.warnings <- false),
  (fun () -> not conf.warnings)
;;

let set_debug_parsing () = ignore (Parsing.set_trace true);;

let get_software_name () = conf.software_name;;
let get_software_version () = conf.software_version;;

let get_definitions_source_file () =
  match conf.definitions_source_file with
  | None -> Path.builtin_source_file
  | Some fname -> fname
;;

let get_overrides_source_file () =
  match conf.overrides_source_file with
  | None -> Path.builtin_source_file
  | Some fname -> fname
;;

let get_source_file_basename () =
  match conf.source_file_basename with
  | None -> Path.builtin_source_file
  | Some fname -> fname
;;

let get_target_file () =
  match conf.target_file with
  | None -> Path.builtin_target_file
  | Some fname -> fname
;;

let set_definitions_source_file fname =
  match conf.definitions_source_file with
  | Some src_name ->
    fatal_error
      (Printf.sprintf
         "source file is already set to \"%s\";        \n  could not set it again to \"%s\""
            src_name fname)
  | None ->
    let tgt_file = get_target_file () in
    if fname = tgt_file && tgt_file <> Path.builtin_target_file then
      fatal_error
        (Printf.sprintf "target file \"%s\"            would overwrite source file \"%s\""
           tgt_file fname) else
    conf.definitions_source_file <- Some fname
;;

let set_source_file_basename fname =
  match conf.source_file_basename with
  | Some src_name ->
    fatal_error
      (Printf.sprintf
         "source file is already set to \"%s\";        \n  could not set it again to \"%s\""
            src_name fname)
  | None ->
      conf.source_file_basename <- Some fname;
      conf.definitions_source_file <-
	Some (Path.add_extension fname definitions_source_file_extension);
      conf.overrides_source_file <-
	Some (Path.add_extension fname overrides_source_file_extension);
;;

let set_overrides_source_file fname =
  match conf.overrides_source_file with
  | Some src_name ->
    fatal_error
      (Printf.sprintf
         "source file is already set to \"%s\";        \n  could not set it again to \"%s\""
            src_name fname)
  | None ->
    let tgt_file = get_target_file () in
    if fname = tgt_file && tgt_file <> Path.builtin_target_file then
      fatal_error
        (Printf.sprintf "target file \"%s\"            would overwrite source file \"%s\""
           tgt_file fname) else
    conf.definitions_source_file <- Some fname
;;

let set_target_file fname =
  match conf.target_file with
  | Some tgt_name ->
    fatal_error
      (Printf.sprintf
         "target file is already set to \"%s\";        \n  could not set it again to \"%s\""
            tgt_name fname)
  | None ->
    let src_file = get_definitions_source_file () in
    if fname = src_file && src_file <> Path.builtin_source_file then
      fatal_error
        (Printf.sprintf "target file \"%s\"            would overwrite source file \"%s\""
           src_file fname) else
    conf.target_file <- Some fname;
;;

let set_prefix str =
  conf.prefix <- Some str;
;;

let default_prefix () =
  match conf.source_file_basename with
  | Some fname -> Some (String.capitalize_ascii fname)
  | None -> None
;;

let get_prefix () =
  match conf.prefix with
  | Some str -> str
  | None ->
      match default_prefix () with
      | Some fname -> fname
      | _ -> failwith "undefined prefix"
;;

let set_prefix_from_object str =
  match conf.prefix with
  | Some _fname ->
      ()
  | None ->
      set_prefix str
;;

let is_definitions_source_file_name fname =
  Path.check_extension fname definitions_source_file_extension
and is_overrides_source_file_name fname =
  Path.check_extension fname overrides_source_file_extension
;;

let report_error ppf = function
  | Fatal_error s ->
      let soft = get_software_name () in
      Format.fprintf ppf
	"@[<v 5>%s: fatal error %s@]@." soft s
;;

let set_path_to_override_for_c fname =
  match conf.path_to_override_for_c with
  | Some src_name ->
      fatal_error
	(Printf.sprintf
           "path to override file is already set to \"%s\";        \n  could not set it again to \"%s\""
            src_name fname)
  | None ->
      conf.path_to_override_for_c <- Some fname
;;

let set_path_to_override_for_h fname =
  match conf.path_to_override_for_h with
  | Some src_name ->
      fatal_error
	(Printf.sprintf
           "path to override file is already set to \"%s\";        \n  could not set it again to \"%s\""
            src_name fname)
  | None ->
      conf.path_to_override_for_h <- Some fname
;;

let get_path_to_override_for_c () =
  match conf.path_to_override_for_c with
  | None -> "codegen/"
  | Some fname -> fname
;;

let get_path_to_override_for_h () =
  match conf.path_to_override_for_h with
  | None -> "codegen/"
  | Some fname -> fname
;;

(* Consistency tests. *)

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
