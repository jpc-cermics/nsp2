(***********************************************************************)
(*                                                                     *)
(*                              OCamlGen                               *)
(*                                                                     *)
(*                       OCaml module Generator                        *)
(*                                                                     *)
(*               EPI Pomdapi, INRIA Paris - Rocquencourt               *)
(*                                                                     *)
(*  Copyright (c) 1997-2015 INRIA.                                     *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved. This file is distributed only by permission.  *)
(*                                                                     *)
(*  Pierre Weis <Pierre.Weis@inria.fr>                                 *)
(*  Francois Clement <Francois.Clement@inria.fr>                       *)
(*  Clement Franchini <Clement.Franchini@inria.fr>                     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** {3 Type definitions for walkers generated with OCamlGen} *)

(* FIXME 4.02: to be removed once OCaml-4.02 is operational. *)
type bytes = string;;

(** {6 Pretty-printer specifications} *)

type 'a top_printer = Format.formatter -> 'a -> unit
and ('a, 'b) top_printer_lifter = 'a top_printer -> 'b top_printer
;;

type 'a printer = Og_precedence.context_kind -> 'a top_printer
and ('a, 'b) printer_lifter =
  'a printer ->
  'b printer
and ('a, 'b, 'c) printer_lifter2 =
  'a printer -> 'b printer ->
  'c printer
and ('a, 'b, 'c, 'd) printer_lifter3 =
  'a printer -> 'b printer -> 'c printer ->
  'd printer
and ('a, 'b, 'c, 'd, 'e) printer_lifter4 =
  'a printer -> 'b printer -> 'c printer -> 'd printer ->
  'e printer
and ('a, 'b, 'c, 'd, 'e, 'f) printer_lifter5 =
  'a printer -> 'b printer -> 'c printer -> 'd printer -> 'e printer ->
  'f printer
and ('a, 'b, 'c, 'd, 'e, 'f, 'g) printer_lifter6 =
  'a printer -> 'b printer -> 'c printer ->
  'd printer -> 'e printer -> 'f printer ->
  'g printer
;;

(** {6 Pretty-scanner specification} *)

type 'a scanner = Scanf.Scanning.in_channel -> 'a
and ('a, 'b) scanner_lifter = 'a scanner -> 'b scanner
and ('a, 'b, 'c) scanner_lifter2 = 'a scanner -> 'b scanner -> 'c scanner
and ('a, 'b, 'c, 'd) scanner_lifter3 =
  'a scanner -> 'b scanner -> 'c scanner -> 'd scanner
;;
