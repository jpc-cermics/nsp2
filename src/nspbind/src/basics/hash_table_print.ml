(***********************************************************************)
(*                                                                     *)
(*                           Simport                                   *)
(*                                                                     *)
(*          Pierre Weis, INRIA Rocquencourt                            *)
(*                                                                     *)
(*  Copyright 2010-2013,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  This file is distributed under the terms of the BSD License.       *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format;;

let print pa pb ppf t =

  let print_binding ppf (k, v) =
    fprintf ppf "@ @[<2>%a =>@ %a@]"
      pa k pb v in

  fprintf ppf "@[<hv 3>{| @[<hv 0>";

  let rec pl ppf = function
    | [] -> fprintf ppf "@]@;<0 -3>|}@]"
    | x :: xs -> fprintf ppf "%a ;%a" print_binding x pl xs in

  pl ppf (Hash_table.bindings t)
;;

let print_t = print;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
