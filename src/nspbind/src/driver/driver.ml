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

let treat_file _ppf src_fname =
  Mdl_compile.compile src_fname
;;

let do_compile = 
  Say.debug "Enter main";
  Main_gen.do_phase "compil" treat_file;;

let main ppf = Arguments.main do_compile ppf;;

main Format.std_formatter;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
