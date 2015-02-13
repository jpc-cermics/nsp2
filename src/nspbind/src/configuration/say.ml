(***********************************************************************)
(*                                                                     *)
(*                               Interface generator                   *)
(*                                                                     *)
(*                   Pierre Weis, INRIA Rocquencourt                   *)
(*                                                                     *)
(*  Copyright 2010-2015,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type message = string
;;

let fdebug f =
  if Configuration.get_debug () then
    Format.fprintf Format.err_formatter
      "%s debug: %a@."
      (Configuration.get_software_name ())
      (fun ppf () -> f ppf) ()
;;

let debug s = fdebug (fun ppf -> Format.fprintf ppf "%s" s);;

let warning s =
  if Configuration.get_warnings () then
  prerr_endline (
    Printf.sprintf "%s warning: %s"
      (Configuration.get_software_name ()) s
  )
;;

let fatal_error s =
  raise (Configuration.Error (Configuration.Fatal_error s))
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
