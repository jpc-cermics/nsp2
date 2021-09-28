(***********************************************************************)
(*                                                                     *)
(*                              OCamlGen                               *)
(*                                                                     *)
(*                       OCaml module Generator                        *)
(*                                                                     *)
(*                       EPI Serena, INRIA Paris                       *)
(*                                                                     *)
(*  Copyright (c) 1997-2018 INRIA.                                     *)
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

(* Code generated for OCaml compiler with version >= 4.02. *)

  type nonrec bytes = bytes;;

(* End of generated code. *)

(** {6 Format string delimiters} *)

type 'a format_delimiter = (unit, 'a, unit) format
(** The type of format string delimiters: format strings used to delimit
  items when printing or scanning.
  A [format_delimiter] is a format string that has no inside conversion.
*)
;;

type 'a listary_format_delimiters =
  {
    fmt_begin : 'a format_delimiter;
    fmt_empty : 'a format_delimiter;
    fmt_separator : 'a format_delimiter;
    fmt_terminator : 'a format_delimiter;
    fmt_end : 'a format_delimiter;
  }
(** The set of delimiters for listary data (['a list] or ['a array]).

  For instance, the Caml conventions for lists are:
  [
  {
    fmt_begin = "[";
    fmt_empty = "[]";
    fmt_separator = ";@ ";
    fmt_terminator = ";";
    fmt_end = "]";
  }
  ]
*)

(** {6 Pretty-printer type specification} *)

type 'a top_printer = Format.formatter -> 'a -> unit
(** An ['a top_printer] is a pretty printer for values of type ['a].

  A [top_printer] is a basic `toplevel' pretty-printer: it always prints its
  argument the same, whatever the printing context could be. For instance, a
  [top_printer] for integers prints integer [-1] verbatim, when in a function
  application context, the argument [-1] should be enclosed by parens and
  [-1] must show as [(-1)].
*)
;;

type 'a printer = Og_precedence.context_kind -> 'a top_printer
(**
  An ['a printer] is a context aware pretty printer for values of type
  ['a].

  In constrast with a [top_printer], a [printer] function uses a context
  indication to tune the way it prints its argument: according to the
  context, the [printer] may print its argument within parens or print
  additional breaks or pretty-printing boxes.

  For instance, a [printer] for integers prints value [-1] verbatim in
  a toplevel context (when [context_kind] argument is [Ck_toplevel]),
  or as [(-1)] in a function argument context (when [context_kind] argument is
  [Ck_argument]),
*)
;;

(** {6 Higher-prder pretty-printer function type specification} *)

type ('a, 'b) printer_lifter =
  'a printer ->
  'b printer
(** An [('a, 'b) printer_lifter] is a [printer] transformer: it maps
  a [printer] for ['a] values to a [printer] for ['b] values.
  For instance an [('a, 'a list) printer_lifter] maps a [printer] for
  values of type ['a] to a [printer] for list of items of type ['a].
*)
;;

type ('a, 'b, 'c) printer_lifter2 =
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
(** Printer lifter for one to six [printer] arguments. *)
;;

type ('a, 'b) top_printer_lifter = 'a top_printer -> 'b top_printer
(** An [('a, 'b) top_printer_lifter] is a [top_printer] transformer: it maps
  a [top_printer] for ['a] values to a [top_printer] for ['b] values.
  For instance an [('a, 'a list) top_printer_lifter] maps a [top_printer] for
  values of type ['a] to a [top_printer] for list of items of type ['a].
*)
;;

(** {6 Pretty-scanner type specification} *)

type 'a scanner = Scanf.Scanning.in_channel -> 'a
(** An ['a scanner] is a scanning function for values of type ['a]. *)
;;

(** {6 Higher-order scanner function type specification} *)

type ('a, 'b) scanner_lifter = 'a scanner -> 'b scanner
(** An [('a, 'b) scanner_lifter] is a [scanner] transformer: it maps
  a [scanner] for ['a] values to a [scanner] for ['b] values.
  For instance, an [('a, 'a list) scanner_lifter] maps a [scanner] for
  values of type ['a] to a [scanner] for list of items of type ['a].
*)
;;

type ('a, 'b, 'c) scanner_lifter2 = 'a scanner -> 'b scanner -> 'c scanner
and ('a, 'b, 'c, 'd) scanner_lifter3 =
  'a scanner -> 'b scanner -> 'c scanner -> 'd scanner
(** Scanner lifter for two and three [scanner] arguments.
  For instance, an [('a, 'b, ('a * 'b)) scanner_lifter2] maps a [scanner] for
  type ['a] and a [scanner] for type ['b] to a [scanner] for pairs of type
  ['a * 'b].
*)
;;
