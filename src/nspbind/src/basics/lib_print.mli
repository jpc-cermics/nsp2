(***********************************************************************)
(*                                                                     *)
(*                              OCamlGen                               *)
(*                                                                     *)
(*                       OCaml module Generator                        *)
(*                                                                     *)
(*               EPI Pomdapi, INRIA Paris - Rocquencourt               *)
(*                                                                     *)
(*  Copyright 1997-2013 INRIA.                                         *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved. This file is distributed only by permission.  *)
(*                                                                     *)
(*  Pierre Weis <Pierre.Weis@inria.fr>                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Runtime library for pretty-printers generated with OCamlGen. *)

open Format_printer;;

val print_unit : unit Format_printer.printer;;
val print_bool : bool printer;;
val print_quoted_string : string printer;;
val print_quoted_char : char printer;;
val print_quoted_int : int printer;;
val print_quoted_int32 : int32 printer;;
val print_quoted_int64 : int64 printer;;
val print_quoted_nativeint : nativeint printer;;
val print_quoted_float : float printer;;

val print_Couple : ('a, 'b, ('a * 'b)) printer_lifter2;;
val print_Triple : ('a, 'b, 'c, ('a * 'b * 'c)) printer_lifter3;;
val print_List : ('a, 'a list) printer_lifter;;
val print_Array : ('a, 'a array) printer_lifter;;
val print_Option : ('a, 'a option) printer_lifter;;
val print_Ref : ('a, 'a ref) printer_lifter;;

val print_fun : ('a, 'b, ('a -> 'b)) printer_lifter2;;

val print_poly : 'a printer;;
val print_abstract : 'a printer;;
val print_external : 'a printer;;

val print_couple :
  (unit, Format.formatter, unit) format ->
  ('a, 'b, ('a * 'b)) printer_lifter2
;;
val print_triple :
  (unit, Format.formatter, unit) format ->
  ('a, 'b, 'c, ('a * 'b * 'c)) printer_lifter3
;;
val print_list :
  (unit, Format.formatter, unit) format ->
  ('a, 'a list) printer_lifter
;;
val print_array :
  (unit, Format.formatter, unit) format ->
  ('a, 'a array) printer_lifter
;;
val print_option :
  (unit, Format.formatter, unit) format ->
  ('a, 'a option) printer_lifter
;;
val print_ref :
  (unit, Format.formatter, unit) format ->
  ('a, 'a ref) printer_lifter
;;

(*
 Local Variables:
  compile-command: "cd ../..; make"
  End:
*)
