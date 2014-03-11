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

let get_usage () =
  let soft = Configuration.get_software_name () in
  Printf.sprintf
    "Usage: %s [options] <filename>\
   \n  options are:"
    soft
;;

(*let usage () = prerr_endline (get_usage ()); exit 1;;*)

let print_version () =
  let soft = Configuration.get_software_name ()
  and version = Configuration.get_interface_version () in
  Printf.eprintf "%s, version %s\n"
    soft version;
  exit 0;
;;

let print_interface_version () =
  Printf.eprintf "%s\n" (Configuration.get_interface_version ());
  exit 0;
;;

let set_general_debug () =
  Configuration.set_debug ();
  Configuration.set_debug_parsing ();
  Configuration.set_debug_slx ()
;;

let executable_file = Sys.argv.(0);;
let which_dir = Filename.dirname executable_file;;
let install_root_dir = Filename.concat which_dir Filename.parent_dir_name;;

let print_which () =
  Printf.eprintf "%s\n" which_dir;
  exit 0;
;;
let print_where () =
  Printf.eprintf "%s\n" (Filename.concat install_root_dir "lib");
  exit 0;
;;

let parse_args () =
  Arg.parse [
    ("-I", Arg.String Path.push,
     "<directory-name>:\
    \n   add the directory name argument to the search path for files");
    ("-d", Arg.Unit Configuration.set_debug,
     ": trigger all debugging flags");
    ("-debug", Arg.Unit set_general_debug,
     ": trigger all debugging flags");
    ("-dparse", Arg.Unit Configuration.set_debug_parsing,
     ": trigger file parsing debugging");
    ("-dslx", Arg.Unit Configuration.set_debug_slx,
     ": trigger SLX debugging; in particular, all files extracted from slx\
    \n   source archive are kept in a separate directory");
    ("-c", Arg.String Configuration.set_source_file,
     ": set the source file argument");
    ("-o", Arg.String Configuration.set_target_file,
     ": set the target file result");
    ("-tl", Arg.String Configuration.set_target_language,
     "<lang>: translate compiled file to <lang> source file\
    \n  (default <lang> is scicoslab).");
    ("-target-language", Arg.String Configuration.set_target_language,
     "<lang>: translate compiled file to <lang> source file\
    \n  (default <lang> is scicoslab).\
    \n   Available languages are:\
    \n   - nsp (to translate to the Nsp language),\
    \n   - scicoslab (to translate to the ScicosLab language),\
    \n   - hypermath (to translate to HyperMath),\
    \n   - octave (to translate to Octave),\
    \n   - e-scicos (to translate to Scicos embedded C-like expressions),\
    \n   - e-simulink (to translate Simulink embedded C-like expressions \
    \n   - matlab (to translate to Matlab),\
    \n   - mtlb (the source language for the Matlab expressions translator).");
    ("-tr", Arg.String Configuration.set_translator,
     "<lang>: translator used to compile the file argument\
    \n  (default <translator> is mdl)");
    ("-translator", Arg.String Configuration.set_translator,
     "<lang>: translator used to compile the file argument\
    \n  (default <translator> is mdl).\
    \n  Available translators are:\
    \n  - mdl (to translate simulink models to scicos),\
    \n  - mtlb (to translate MatLab programs to NSP, ScicosLab, etc),\
    \n  - sime (to translate Simulink embedded C-like expressions \
    \n    to NSP, ScicosLab, etc.)");
    ("-ccf", Arg.String Configuration.set_companion_source_file,
     ": set the MatLab companion source file");
    ("-ocf", Arg.String Configuration.set_companion_target_file,
     ": set the Matlab companion translated file");
    ("-verbose", Arg.Unit Configuration.set_verbose,
     ": set the verbose mode");
    ("-v", Arg.Unit Configuration.set_verbose,
     ": set the verbose mode");
    ("-nw", Arg.Unit Configuration.set_no_warnings,
     ": set the no warnings mode");
    ("-no-warnings", Arg.Unit Configuration.set_no_warnings,
     ": set the no warnings mode");
    ("-version", Arg.Unit print_version,
     ": print the version of simport");
    ("-interface-version", Arg.Unit print_interface_version,
     ":\
    \n   print the version of model interfaces compatible with\
    \n   this version of simport");
    ("-where", Arg.Unit print_where,
     ":\
    \n   print absolute directory of simport library.");
    ("-which", Arg.Unit print_which,
     ":\
    \n   print absolute directory of simport executable.");
  ]
  Configuration.set_source_file
  (get_usage ())
;;

let main do_file ppf =
  try
    Path.init (Configuration.get_search_path ());
    parse_args ();
    Configuration.init_and_check ();
    let fname = Configuration.get_source_file () in
    do_file ppf fname
  with
  | Configuration.Error err ->
    Configuration.report_error Format.err_formatter err
;;


(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
