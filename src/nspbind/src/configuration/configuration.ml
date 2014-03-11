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

let fatal_error s = raise (Error (Fatal_error s));;

let print_version v = Format.fprintf Format.std_formatter "%s" v;;
let print_interface_version = print_version;;

let simulink_translator_name = "simport";;
let simulink_translator_version = "0.9.0";;
let simulink_interface_version = "1.0.0";;

let mdl_source_file_extension = Path.to_file_extension ".mdl"
and nsp_source_file_extension = Path.to_file_extension ".nsp"
and scicoslab_source_file_extension = Path.to_file_extension ".sci"
and matlab_source_file_extension = Path.to_file_extension ".m"
and octave_source_file_extension = Path.to_file_extension ".oct"
and hypermath_source_file_extension = Path.to_file_extension ".hml"
and mtlb_source_file_extension = Path.to_file_extension ".override"

and scicos_mbd_source_file_extension = Path.to_file_extension ".cose"
and sime_source_file_extension = Path.to_file_extension ".sime"
;;

let mdl_implementation_source_file_extension = mdl_source_file_extension
and slx_implementation_source_file_extension = Path.to_file_extension ".slx"
and xml_implementation_source_file_extension = Path.to_file_extension ".xml"

and simulink_implementation_marshaled_file_extension = Path.to_file_extension ".mdlo"
and simulink_companion_source_file_extension = matlab_source_file_extension

and simulink_interface_source_file_extension = Path.to_file_extension ".mdli"
and simulink_interface_marshaled_file_extension = Path.to_file_extension ".mdlio"
;;

let sime_translated_file_extension = scicos_mbd_source_file_extension;;

let source_file_extension_of_mdl_target_language = function
  | ScicosLab -> scicoslab_source_file_extension
  | Nsp -> nsp_source_file_extension
  | Hypermath -> hypermath_source_file_extension
;;

let source_file_extension_of_mtlb_target_language = function
  | Octave -> octave_source_file_extension
  | Matlab -> matlab_source_file_extension
  | Scicos_host lang -> source_file_extension_of_mdl_target_language lang
;;

let source_file_extension_of_sime_target_language = function
  | E_scicos -> scicos_mbd_source_file_extension
;;

let source_file_extension_of_target_language = function
  | Target_simulink lang ->
    source_file_extension_of_mdl_target_language lang
  | Target_mtlb lang ->
    source_file_extension_of_mtlb_target_language lang
  | Target_sime lang ->
    source_file_extension_of_sime_target_language lang
;;

let source_file_extension_of_mdl_source_language = function
  | Mdl -> mdl_implementation_source_file_extension
  | Slx -> slx_implementation_source_file_extension
  | Xml -> xml_implementation_source_file_extension
;;

let source_file_extension_of_mtlb_source_language = function
  | Mtlb -> octave_source_file_extension
;;

let source_file_extension_of_sime_source_language = function
  | E_simulink -> sime_source_file_extension
;;

let source_file_extension_of_source_language = function
  | Source_simulink lang ->
    source_file_extension_of_mdl_source_language lang
  | Source_mtlb lang ->
    source_file_extension_of_mtlb_source_language lang
  | Source_sime lang ->
    source_file_extension_of_sime_source_language lang
;;

let simulink_source_language_of_string = function
  | "mdl" | "MDL" | "simulink" | "Simulink" -> Mdl
  | "xml" | "XML" -> Xml
  | "slx" | "SLX" -> Slx
  | _ -> raise Not_found
;;

let sime_source_language_of_string = function
  | "e-simulink" | "embedded-simulink" | "E-Simulink" -> E_simulink
  | _ -> raise Not_found
;;

let mtlb_source_language_of_string = function
  | "mtlb" | "Mtlb" -> Mtlb
  | _ -> raise Not_found
;;

let source_language_of_string lang =
  try Source_simulink (simulink_source_language_of_string lang) with
  | Not_found ->
    try Source_sime (sime_source_language_of_string lang) with
    | Not_found ->
      Source_mtlb (mtlb_source_language_of_string lang)
;;

let simulink_target_language_of_string = function
  | "scicoslab" | "Scicoslab" -> ScicosLab
  | "nsp" | "Nsp" -> Nsp
  | "hypermath" | "Hypermath" -> Hypermath
  | _ -> raise Not_found
;;

let sime_target_language_of_string = function
  | "e-scicos" | "embedded-scicos" | "E-Scicos" -> E_scicos
  | _ -> raise Not_found
;;

let mtlb_target_language_of_string = function
  | "octave" | "Octave" -> Octave
  | "matlab" | "Matlab" -> Matlab
  | lang -> Scicos_host (simulink_target_language_of_string lang)
;;

let target_language_of_string lang =
  try Target_simulink (simulink_target_language_of_string lang) with | Not_found ->
  try Target_sime (sime_target_language_of_string lang) with | Not_found ->
  Target_mtlb (mtlb_target_language_of_string lang)
;;

let string_of_simulink_target_language = function
  | ScicosLab -> "scicoslab"
  | Nsp -> "nsp"
  | Hypermath -> "hypermath"
;;

let string_of_mtlb_target_language = function
  | Octave -> "octave"
  | Matlab -> "matlab"
  | Scicos_host lang -> string_of_simulink_target_language lang
;;

let string_of_sime_target_language = function
  | E_scicos -> "e-scicos"
;;

let string_of_target_language = function
  | Target_simulink lang ->
    string_of_simulink_target_language lang
  | Target_mtlb lang ->
    string_of_mtlb_target_language lang
  | Target_sime lang ->
    string_of_sime_target_language lang
;;

(* let string_of_simulink_source_language = function *)
(*   | Mdl -> "mdl" *)
(*   | Xml -> "xml" *)
(*   | Slx -> "slx" *)
(* ;; *)

(* let string_of_mtlb_source_language = function *)
(*   | Mtlb -> "mtlb" *)
(* ;; *)

(* let string_of_sime_source_language = function *)
(*   | E_simulink -> "E-simulink" *)
(* ;; *)

(* let string_of_source_language = function *)
(*   | Source_simulink lang -> *)
(*     string_of_simulink_source_language lang *)
(*   | Source_mtlb lang -> *)
(*     string_of_mtlb_source_language lang *)
(*   | Source_sime lang -> *)
(*     string_of_sime_source_language lang *)
(* ;; *)

(* let string_of_language = function *)
(*   | Source_language lang -> *)
(*     string_of_source_language lang *)
(*   | Target_language lang -> *)
(*     string_of_target_language lang *)
(* ;; *)

(* let simulink_pervasives_interface = Ident.Ident "simport_pervasives";; *) 

let simulink_pervasives_interface =  "simport_pervasives";;

(** Builtins *)
let builtin_model_ident = "BlockDiagram";;
let builtin_source_language = Source_simulink Mdl;;
let builtin_target_language = Target_simulink ScicosLab;;

let builtin_translator = T_simulink;;

let natural = Unatural_types.to_natural;;

let builtin_mean_blocks_by_system = natural 59
and builtin_mean_block_bindings = Unatural._7_
and builtin_mean_block_types = natural 101
and builtin_mean_block_parameters = Unatural._3_
;;

let builtin_font = {
  Simport_configuration.font_name = "Helvetica";
  font_angle = "normal";
  font_size = natural 10;
  font_weight = "normal";
}
;;

let conf = {
  Simport_configuration.
  software_name = simulink_translator_name;
  software_version = simulink_translator_version;
  interface_version = simulink_interface_version;

  verbose = false;
  debug = false;
  debug_slx = false;
  warnings = true;

  search_path = Path.builtin_search_path;

  model_ident = None;

  source_file = None;
  companion_source_file = None;

  target_file = None;
  companion_target_file = None;

  translator = None;

  interfaces = [];

  source_language = None;
  target_language = None;

  window_width = natural 1920;
  window_height = natural 1200;

  diagram_left_margin = natural 10;
  diagram_right_margin = natural 450;
  diagram_bottom_margin = natural 10;

  default_font = builtin_font;

  mean_blocks_by_system = builtin_mean_blocks_by_system;
  mean_block_bindings = builtin_mean_block_bindings;
  max_block_types = builtin_mean_block_types;
  mean_block_parameters = builtin_mean_block_parameters;

  block_vertical_spacing = natural 50;
  block_port_width = natural 15;
  block_port_height = natural 10;
  block_geometry_magnification = natural 100;

  not_yet_translated_mark = "?";

  model_target_ident = "scsm";
}
;;

let get_translator () =
  match conf.translator with
  | Some tr -> tr
  | None -> builtin_translator
;;

let translator_of_string = function
  | "mdl" | "simulink" | "Simulink"
  | "xml" | "slx" -> T_simulink
  | "mtlb" | "matlab" | "Matlab" -> T_mtlb
  | "sime" | "e-simulink" | "simulink-embedded-expression" -> T_sime
  | trans ->
    fatal_error (
      Printf.sprintf "translator %S is unknown" trans
    )
;;

let string_of_translator = function
  | T_simulink -> "simulink"
  | T_mtlb -> "mtlb"
  | T_sime -> "sime"
;;

let set_translator trans =
  match conf.translator with
  | Some tr ->
    fatal_error
      (Printf.sprintf
         "translator is already set to \"%s\";        \n  could not set it again to \"%s\""
            (string_of_translator tr) trans)
  | None ->
    let tr = translator_of_string trans in
    conf.translator <- Some tr
;;

let translator_of_source_file_extension ext =
  match Path.of_file_extension ext with
  | ".mdl" | ".mdli" | ".mdlo"
  | ".xml" | ".slx" -> Some T_simulink
  | ".mtlb" | ".m" -> Some T_mtlb
  | ".sime" -> Some T_sime
  | _ -> None
;;

let translator_of_target_file_extension ext =
  match Path.of_file_extension ext with
  | ".sci" | ".nsp" | ".hml"
  | ".mdlo" | ".mdlio" -> Some T_simulink
  | ".m" | ".oct" -> Some T_mtlb
  | ".cose" -> Some T_sime
  | _ -> None
;;

let translator_of_companion_file_extension ext =
  match Path.of_file_extension ext with
  | ".m" -> Some T_mtlb
  | _ -> None
;;

let lift_to_option f = function
  | None -> None
  | Some x -> f x
;;

let translator_of_source_file_name efname =
  translator_of_source_file_extension (Path.get_extension efname)
;;

let translator_of_target_file_name efname =
  translator_of_target_file_extension (Path.get_extension efname)
;;

let translator_of_companion_file_name efname =
  translator_of_companion_file_extension (Path.get_extension efname)
;;

(*
let translator_of_source_file =
  lift_to_option translator_of_source_file_name
;;

let translator_of_target_file =
  lift_to_option translator_of_target_file_name
;;
*)
let translator_of_companion_file =
  lift_to_option translator_of_companion_file_name
;;

let get_target_language () =
  match conf.target_language with
  | Some tr -> tr
  | None -> builtin_target_language
;;

let set_target_language lang =
  conf.target_language <- Some (target_language_of_string lang)
;;

let get_source_language () =
  match conf.source_language with
  | Some tr -> tr
  | None -> builtin_source_language
;;

let set_source_language lang =
  conf.source_language <- Some (source_language_of_string lang)
;;

let get_interfaces () = List.rev conf.interfaces;;
let add_interface int_id = conf.interfaces <- int_id :: conf.interfaces;;

let add_simulink_pervasives_interface () =
  add_interface simulink_pervasives_interface
;;

let get_search_path () = conf.search_path;;
let set_search_path p = conf.search_path <- p;;

let set_verbose, get_verbose =
  (fun () -> conf.verbose <- true),
  (fun () -> conf.verbose)
;;

let set_debug, get_debug =
  (fun () -> conf.debug <- true),
  (fun () -> conf.debug)
;;

let set_debug_slx, get_debug_slx =
  (fun () -> conf.debug_slx <- true),
  (fun () -> conf.debug_slx)
;;

let set_warnings, get_warnings =
  (fun () -> conf.warnings <- true),
  (fun () -> conf.warnings)
;;

let set_no_warnings, get_no_warnings =
  (fun () -> conf.warnings <- false),
  (fun () -> not conf.warnings)
;;

let set_default_font, get_default_font =
  (fun fnt -> conf.default_font <- fnt),
  (fun () -> conf.default_font)
;;

let set_debug_parsing () = ignore (Parsing.set_trace true);;

let get_software_name () = conf.software_name;;
let get_software_version () = conf.software_version;;
let get_interface_version () = conf.interface_version;;

let get_source_file () =
  match conf.source_file with
  | None -> Path.builtin_source_file
  | Some fname -> fname
;;

let get_target_file () =
  match conf.target_file with
  | None -> Path.builtin_target_file
  | Some fname -> fname
;;

let get_companion_source_file_option () = conf.companion_source_file;;
let get_companion_source_file () =
  match conf.companion_source_file with
  | None -> Path.builtin_source_file
  | Some fname -> fname
;;

let get_companion_target_file () =
  match conf.companion_target_file with
  | None -> Path.builtin_target_file
  | Some fname -> fname
;;


let set_source_file fname =
  match conf.source_file with
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
    conf.source_file <- Some fname
;;

let set_target_file fname =
  match conf.target_file with
  | Some tgt_name ->
    fatal_error
      (Printf.sprintf
         "target file is already set to \"%s\";        \n  could not set it again to \"%s\""
            tgt_name fname)
  | None ->
    let src_file = get_source_file () in
    if fname = src_file && src_file <> Path.builtin_source_file then
      fatal_error
        (Printf.sprintf "target file \"%s\"            would overwrite source file \"%s\""
           src_file fname) else
    conf.target_file <- Some fname;
;;

let set_companion_source_file fname =
  match conf.companion_source_file with
  | Some src_name ->
    fatal_error
      (Printf.sprintf
         "companion source file is already set to \"%s\";        \n  could not set it again to \"%s\""
            src_name fname)
  | None ->
    let tgt_file = get_companion_target_file () in
    if fname = tgt_file && tgt_file <> Path.builtin_target_file then
      fatal_error
        (Printf.sprintf "target file \"%s\"            would overwrite companion source file \"%s\""
           tgt_file fname) else
    conf.companion_source_file <- Some fname
;;

let set_companion_target_file fname =
  match conf.companion_target_file with
  | Some tgt_name ->
    fatal_error
      (Printf.sprintf
         "companion target file is already set to \"%s\";        \n  could not set it again to \"%s\""
            tgt_name fname)
  | None ->
    let src_file = get_companion_source_file () in
    if fname = src_file && src_file <> Path.builtin_source_file then
      fatal_error
        (Printf.sprintf "companion target file \"%s\"            would overwrite companion source file \"%s\""
           src_file fname) else
      conf.companion_target_file <- Some fname;
;;

let get_mean_blocks_by_system () = conf.mean_blocks_by_system;;
let get_mean_block_bindings () = conf.mean_block_bindings;;
let get_max_block_types () = conf.max_block_types;;
let get_mean_block_parameters () = conf.mean_block_parameters;;

let get_window_width () = conf.window_width;;
let get_window_height () = conf.window_height;;

let get_diagram_left_margin () = conf.diagram_left_margin;;
let get_diagram_right_margin () = conf.diagram_right_margin;;
let get_diagram_bottom_margin () = conf.diagram_bottom_margin;;
let get_block_vertical_spacing () = conf.block_vertical_spacing;;

let get_block_port_width () = conf.block_port_width;;
let get_block_port_height () = conf.block_port_height;;
let get_block_geometry_magnification () = conf.block_geometry_magnification;;
let set_block_port_width w = conf.block_port_width <- w;;
let set_block_port_height h = conf.block_port_height <- h;;
let set_block_geometry_magnification m =
  conf.block_geometry_magnification <- m
;;

let get_not_yet_translated_mark () = conf.not_yet_translated_mark;;

let get_model_target_ident () = conf.model_target_ident;;

let add_source_file_extension_of_target_language efname =
  let target_extension =
    source_file_extension_of_target_language (get_target_language ()) in
  Path.change_explicit_extension efname target_extension
;;

(*let add_mdl_implementation_source_extension fname =
  Path.add_extension mdl_implementation_source_file_extension fname
;;

let add_target_language_source_file_extension =
  Path.add_extension conf.target_language_source_file_extension
;;
let add_implementation_translated_file_extension =
  add_target_language_source_file_extension
;;

let add_mtlb_translated_file_extension =
  add_target_language_source_file_extension
;;
let add_sime_translated_file_extension =
  add_target_language_source_file_extension
;;
*)

let is_mdl_implementation_source_file_name fname =
  Path.check_extension fname mdl_implementation_source_file_extension
and is_xml_implementation_source_file_name fname =
  Path.check_extension fname xml_implementation_source_file_extension
and is_slx_implementation_source_file_name fname =
  Path.check_extension fname slx_implementation_source_file_extension
and is_mdl_implementation_source_explicit_file_name efname =
  Path.check_explicit_extension efname mdl_implementation_source_file_extension
and is_xml_implementation_source_explicit_file_name efname =
  Path.check_explicit_extension efname xml_implementation_source_file_extension
and is_slx_implementation_source_explicit_file_name efname =
  Path.check_explicit_extension efname slx_implementation_source_file_extension

and is_simulink_interface_source_file_name fname =
  Path.check_extension fname simulink_interface_source_file_extension
and is_simulink_companion_source_file_name fname =
  Path.check_extension fname matlab_source_file_extension

and is_matlab_source_file_name fname =
  Path.check_extension fname matlab_source_file_extension
and is_matlab_source_explicit_file_name efname =
  Path.check_explicit_extension efname matlab_source_file_extension
and is_mtlb_source_file_name fname =
  Path.check_extension fname mtlb_source_file_extension
and is_mtlb_source_explicit_file_name efname =
  Path.check_explicit_extension efname mtlb_source_file_extension
and is_sime_source_file_name fname =
  Path.check_extension fname sime_source_file_extension
and is_sime_source_explicit_file_name efname =
  Path.check_explicit_extension efname sime_source_file_extension
;;

let is_simulink_implementation_source_file_name fname =
  is_mdl_implementation_source_file_name fname ||
  is_xml_implementation_source_file_name fname ||
  is_slx_implementation_source_file_name fname
and is_simulink_implementation_source_explicit_file_name efname =
  is_mdl_implementation_source_explicit_file_name efname ||
  is_xml_implementation_source_explicit_file_name efname ||
  is_slx_implementation_source_explicit_file_name efname
;;

let is_simulink_translator_source_file_name fname =
  is_simulink_interface_source_file_name fname ||
  is_simulink_companion_source_file_name fname ||
  is_simulink_implementation_source_file_name fname

and is_mtlb_translator_source_file_name fname =
  is_matlab_source_file_name fname ||
  is_mtlb_source_file_name fname
and is_mtlb_translator_source_explicit_file_name efname =
  is_matlab_source_explicit_file_name efname ||
  is_mtlb_source_explicit_file_name efname
;;

let is_simulink_implementation_marshaled_file_name fname =
  Path.check_extension fname simulink_implementation_marshaled_file_extension
and is_simulink_implementation_marshaled_explicit_file_name efname =
  Path.check_explicit_extension
    efname simulink_implementation_marshaled_file_extension
and is_simulink_interface_marshaled_file_name fname =
  Path.check_extension fname simulink_interface_marshaled_file_extension
;;

let is_simulink_implementation_translated_file_name fname =
  Path.check_extension fname scicoslab_source_file_extension ||
  Path.check_extension fname nsp_source_file_extension ||
  Path.check_extension fname hypermath_source_file_extension

and is_sime_translated_file_name fname =
  Path.check_extension fname sime_translated_file_extension

and is_mtlb_translated_file_name fname =
  Path.check_extension fname scicoslab_source_file_extension ||
  Path.check_extension fname nsp_source_file_extension ||
  Path.check_extension fname hypermath_source_file_extension ||
  Path.check_extension fname octave_source_file_extension ||
  Path.check_extension fname matlab_source_file_extension
;;

let simulink_implementation_translated_file_name efname =
  if not (is_simulink_implementation_source_explicit_file_name efname) &&
    not (is_simulink_implementation_marshaled_explicit_file_name efname) then
    failwith (Printf.sprintf
      "%s is not a valid Simulink implementation file name"
      (Path.of_explicit_file_name efname));
  add_source_file_extension_of_target_language efname
;;

let matlab_translated_file_name efname =
  if not (is_matlab_source_explicit_file_name efname) then
    failwith (Printf.sprintf
      "%s is not a valid matlab source file name"
      (Path.of_explicit_file_name efname));
  add_source_file_extension_of_target_language efname
;;

let mtlb_translated_file_name efname =
  if not (is_mtlb_translator_source_explicit_file_name efname) then
    failwith (Printf.sprintf
      "%s is not a valid mtlb source file name"
      (Path.of_explicit_file_name efname));
  add_source_file_extension_of_target_language efname
;;

let sime_translated_file_name efname =
  if not (is_sime_source_explicit_file_name efname) then
    failwith (Printf.sprintf
      "%s is not a valid simulink_expression source file name"
      (Path.of_explicit_file_name efname));
  Path.change_explicit_extension efname sime_translated_file_extension
;;

let simulink_interface_marshaled_file_name efname =
  Path.change_explicit_extension efname simulink_interface_marshaled_file_extension
;;

let simulink_implementation_marshaled_file_name efname =
  Path.change_explicit_extension efname simulink_implementation_marshaled_file_extension
;;

(* Consistency tests. *)

(* Target languages. *)

let default_sime_target_language = E_scicos;;
let default_simulink_target_language = Nsp;;
let default_mtlb_target_language = Scicos_host Nsp;;

let check_sime_target_language tl =
  match tl with
  | None ->
    conf.target_language <- Some (Target_sime default_sime_target_language)
  | Some target_language ->
    match target_language with
    | Target_simulink _ | Target_mtlb _ ->
      fatal_error (Printf.sprintf
        "sime target language should be E_scicos,          it can not be %s"
        (string_of_target_language target_language))
    | Target_sime E_scicos -> ()
;;

let check_simulink_target_language tl =
  match tl with
  | None ->
    conf.target_language <-
      Some (Target_simulink default_simulink_target_language)
  | Some target_language ->
    match target_language with
    | Target_simulink _ -> ()
    | Target_mtlb (Scicos_host (ScicosLab | Nsp | Hypermath)) -> ()
    | Target_mtlb (Octave | Matlab) | Target_sime _ ->
      fatal_error (Printf.sprintf
        "simulink target language can not be %s"
        (string_of_target_language target_language))
;;

let rec check_mtlb_target_language tl =
  match tl with
  | None ->
    conf.target_language <- Some (Target_mtlb default_mtlb_target_language)
  | Some target_language ->
    match target_language with
    | Target_simulink lang ->
      check_mtlb_target_language (Some (Target_mtlb (Scicos_host lang)))
    | Target_sime _ ->
      fatal_error (Printf.sprintf
        "mtlb target language should be one of Nsp, HyperMath, ScicosLab,\n         Matlab, or Octave; it can not be %s"
        (string_of_target_language target_language))
    | Target_mtlb _ -> ()
;;

(* Source and target files consistency checks. *)

let check_sime_src_tgt_files sf tf =
  if not (is_sime_source_file_name sf) then
    fatal_error (Printf.sprintf
      "sime target file should have extension %s"
      (Path.string_of_extension sime_source_file_extension)
    ) else
  if not (is_sime_translated_file_name tf) then
    fatal_error (Printf.sprintf
      "sime source file should have extension %s"
      (Path.string_of_extension sime_source_file_extension)
    )
;;

let check_simulink_src_tgt_files (sf : Path.file_name) (tf : Path.file_name) =
  if is_simulink_interface_source_file_name sf then
    if not (is_simulink_interface_marshaled_file_name tf) then
      fatal_error (Printf.sprintf
        "mdl compiled interface file should have extension %s"
        (Path.string_of_extension simulink_interface_marshaled_file_extension)
      )
    else () else
  if is_simulink_implementation_source_file_name sf ||
    is_simulink_implementation_marshaled_file_name sf ||
    is_simulink_companion_source_file_name sf then
    if not (is_simulink_implementation_translated_file_name tf) then
      fatal_error (Printf.sprintf
        "%s is not a valid simulink compiled implementation file name;        \n it should have extension %s"
        tf
        (Path.string_of_extension simulink_implementation_marshaled_file_extension)
      ) else () else
  fatal_error (Printf.sprintf
    "%s is not a valid simport source file; its extension should be one of %s"
    sf ".mdl, .mdli, .mdlo, .xml, .slx or .m"
  )
;;

let check_mtlb_src_tgt_files sf tf =
  if is_mtlb_source_file_name sf ||
     is_matlab_source_file_name sf then
    if not (is_mtlb_translated_file_name tf) then
      fatal_error (Printf.sprintf
        "%s is not a valid mtlb translated file name;       \n its extension should be one of %s."
        tf ".sci, .nsp, .hml, .oct, or .m"
      ) else () else
  fatal_error (Printf.sprintf
    "%s is not a valid mtlb source file; its extension should be one of %s."
    sf ".mtlb, or .m")
;;

let fill_simulink_src_tgt_files _sf _tf = ()
;;

let fill_mtlb_src_tgt_files _sf _tf = ()
;;

let fill_sime_src_tgt_files _sf _tf = ()
;;

let set_translation_option opt =
  match conf.translator with
  | None -> conf.translator <- opt
  | Some _ -> assert false
;;

let check_and_fill_translator tr =

  match tr with
  | None ->
    let tr =
      let src_file_opt = conf.source_file in
      let tgt_file_opt = conf.target_file in
      (match src_file_opt, tgt_file_opt with
       | None, None -> translator_of_companion_file conf.companion_source_file
       | None, Some tgt_name ->
         (match translator_of_target_file_name tgt_name with
          | None -> translator_of_companion_file conf.companion_source_file
          | Some _ as tr -> tr
         )
       | Some src_name, None ->
         (match translator_of_source_file_name src_name with
          | None -> translator_of_companion_file conf.companion_source_file
          | Some _ as tr -> tr
         )
       | Some src_name, Some tgt_name ->
         (match translator_of_source_file_name src_name with
          | None ->
            (match translator_of_target_file_name tgt_name with
             | None -> translator_of_companion_file conf.companion_source_file
             | Some _ as tr -> tr
            )
          | Some tr_src as tr ->
            (match translator_of_target_file_name tgt_name with
             | None -> translator_of_companion_file conf.companion_source_file
             | Some tr_tgt ->
               if tr_src = tr_tgt then tr else
               translator_of_companion_file conf.companion_source_file)
         )
      ) in
    set_translation_option tr
  | Some _tr -> ()
;;

let init_and_check () =

  add_simulink_pervasives_interface ();

  check_and_fill_translator conf.translator;

  let tr = get_translator ()

  and sf = conf.source_file
  and tf = conf.target_file in
  match tr with
  | T_simulink ->
    check_simulink_target_language conf.target_language;
    fill_simulink_src_tgt_files sf tf;
    check_simulink_src_tgt_files (get_source_file ()) (get_target_file ())
  | T_mtlb ->
    check_mtlb_target_language conf.target_language;
    fill_mtlb_src_tgt_files sf tf;
    check_mtlb_src_tgt_files (get_source_file ()) (get_target_file ())
  | T_sime ->
    check_sime_target_language conf.target_language;
    fill_sime_src_tgt_files sf tf;
    check_sime_src_tgt_files (get_source_file ()) (get_target_file ())
;;

let report_error ppf = function
  | Fatal_error s ->
    let soft = get_software_name () in
    Format.fprintf ppf
      "@[<v 5>%s: fatal error %s@]@." soft s
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
