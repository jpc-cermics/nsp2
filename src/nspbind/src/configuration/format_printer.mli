(***********************************************************************)
(*                                                                     *)
(*                              OCamlGen                               *)
(*                                                                     *)
(*                       OCaml module Generator                        *)
(*                                                                     *)
(*               EPI Pomdapi, INRIA Paris - Rocquencourt               *)
(*                                                                     *)
(*  Copyright 1997-2015 INRIA.                                         *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved. This file is distributed only by permission.  *)
(*                                                                     *)
(*  Pierre Weis <Pierre.Weis@inria.fr>                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Runtime library for pretty-printers generated with ocamlprintc. *)

type 'a printer = Format.formatter -> 'a -> unit;;

type 'a eta_printer = 'a printer -> 'a -> unit;;

type ('a, 'b) printer_lifter = 'a printer -> 'b printer;;

type ('a, 'b, 'c) printer_lifter2 = 'a printer -> 'b printer -> 'c printer;;

type ('a, 'b, 'c, 'd) printer_lifter3 =
  'a printer -> 'b printer -> 'c printer -> 'd printer
;;

type 'a t = 'a printer;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
