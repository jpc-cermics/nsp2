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

let compile _src_fname =
  let dst_efname = Configuration.get_target_file () in 
  let src_overrides_efname = Path.find (Configuration.get_overrides_source_file ()) in 
  let src_definitions_efname = Path.find (Configuration.get_definitions_source_file ()) in 
  Override_print.overrides_init src_overrides_efname;
  Override_print.definitions_init src_definitions_efname;
  Path.with_out_file dst_efname 
    (fun oc ->
      let ppf = Format.formatter_of_out_channel oc in
      Format.fprintf ppf "%a@."
	Override_print.translate_file dst_efname)
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
