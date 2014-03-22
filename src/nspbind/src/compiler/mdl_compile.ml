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

let write_companion_translated_source_file () = 
  let dst_efname = Configuration.get_target_file () in 
  let src_efname = Path.find (Configuration.get_overrides_source_file ()) in 
  Path.with_out_file dst_efname 
    (fun oc ->
      let ppf = Format.formatter_of_out_channel oc in
      Format.fprintf ppf "%a@."
	Override_print.translate_file src_efname);
  
;;

let compile _src_fname =
  write_companion_translated_source_file ()
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
