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

(** {3 Runtime library for pretty-printers generated with OCamlGen} *)

open Og_types;;

val escape_char : char -> string;;
val escape_string : string -> string;;

(** {6 Basic printers} *)

val printer_Unit : unit printer;;
val printer_Bool : bool printer;;
val printer_Char : char printer;;
val printer_String : string printer;;
val printer_Bytes : Og_types.bytes printer;;

val printer_Int : int printer;;
val printer_Int32 : int32 printer;;
val printer_Int64 : int64 printer;;
val printer_Nativeint : nativeint printer;;
val printer_Float : float printer;;

(** {6 Printers for basic polymorphic values} *)

val printer_Format :
  ('a, 'b, 'c, ('a, 'b, 'c) format) printer_lifter3
;;
val printer_Format4 :
  ('a, 'b, 'c, 'd, ('a, 'b, 'c, 'd) format4) printer_lifter4
;;
val printer_Format6 :
  ('a, 'b, 'c, 'd, 'e, 'f, ('a, 'b, 'c, 'd, 'e, 'f) format6) printer_lifter6
;;

val printer_List : ('a, 'a list) printer_lifter;;
val printer_Array : ('a, 'a array) printer_lifter;;

val printer_Option : ('a, 'a option) printer_lifter;;
val printer_Ref : ('a, 'a ref) printer_lifter;;

(** {6 Printers for fully abstract values} *)

val printer_Fun : ('a, 'b, 'a -> 'b) printer_lifter2;;
val printer_Poly : 'a printer;;
val printer_Abstract : 'a printer;;
val printer_External : 'a printer;;
val printer_Lazy : unit printer;;
