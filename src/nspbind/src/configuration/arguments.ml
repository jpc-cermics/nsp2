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
  and version = Configuration.get_software_version () in
  Printf.eprintf "%s, version %s\n"
    soft version;
  exit 0;
;;

let set_general_debug () =
  Configuration.set_debug ();
  Configuration.set_debug_parsing ();
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
    ("-c", Arg.String Configuration.set_source_file_basename,
     ": set the source file argument");
    ("-o", Arg.String Configuration.set_target_file,
     ": set the target file result");
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
    ("-where", Arg.Unit print_where,
     ":\
    \n   print absolute directory of simport library.");
    ("-which", Arg.Unit print_which,
     ":\
    \n   print absolute directory of simport executable.");
  ]
  Configuration.set_source_file_basename
  (get_usage ())
;;

let main do_file ppf =
  try
    Path.init Path.builtin_search_path;
    parse_args ();
    (* Configuration.init_and_check (); *)
    let fname = Configuration.get_source_file_basename () in
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
